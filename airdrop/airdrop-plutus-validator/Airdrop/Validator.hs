{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

-- | Marlowe semantics validator.
module Airdrop.Validator (
  Hash,
  Root (..),
  Proof,
  TokenAllocation,
  Withdrawal,
  withdraw,
  compileValidator,
  hash,
  validator,
  combineHash,
  hashTokenAllocation,
  unusableTokenAllocation,
) where

import qualified Cardano.Crypto.Hash as Hash
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import GHC.Generics (Generic)
import PlutusCore.Core (plcVersion100)
import PlutusLedgerApi.V1 (ToData)
import PlutusLedgerApi.V1.Scripts (DatumHash)
import PlutusLedgerApi.V2 (
  CurrencySymbol,
  FromData,
  OutputDatum (OutputDatumHash),
  PubKeyHash (..),
  ScriptHash (..),
  SerialisedScript,
  TxInInfo (TxInInfo, txInInfoResolved),
  TxOut (TxOut, txOutAddress, txOutDatum, txOutValue),
  UnsafeFromData (unsafeFromBuiltinData),
  Value (Value, getValue),
  serialiseCompiledCode,
  toBuiltinData,
 )
import PlutusLedgerApi.V2.Contexts (ScriptPurpose (..), txInInfoOutRef)
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AssocMap
import PlutusTx.Builtins (mkI, serialiseData)
import PlutusTx.Prelude
import qualified Prelude as Haskell

type Hash = BuiltinByteString

type Proof = [Either Hash Hash]

type TokenAllocation = (PubKeyHash, Integer)

invalidPubKeyHash :: PubKeyHash
invalidPubKeyHash = PubKeyHash ""

unusableTokenAllocation :: TokenAllocation
unusableTokenAllocation = (invalidPubKeyHash, -1)

unusableTokenAllocationHash :: Hash
unusableTokenAllocationHash = hashTokenAllocation unusableTokenAllocation

hash :: BuiltinByteString -> Hash
hash = sha2_256
{-# INLINEABLE hash #-}

hashTokenAllocation :: TokenAllocation -> Hash
hashTokenAllocation (PubKeyHash addr, amount) = do
  let -- The most efficient way to serialize the integer:
      -- https://github.com/IntersectMBO/plutus/issues/3657#issuecomment-1440944698
      amountByteString = serialiseData . mkI $ amount
  hash $ addr `appendByteString` "#" `appendByteString` amountByteString
{-# INLINEABLE hashTokenAllocation #-}

combineHash :: Hash -> Hash -> Hash
combineHash h h' = hash (appendByteString h h')
{-# INLINEABLE combineHash #-}

newtype Root = Root Hash
  deriving newtype (Eq, ToData, UnsafeFromData, FromData, Haskell.Show)

type Withdrawal = (TokenAllocation, Proof)

withdraw :: Withdrawal -> Root -> Maybe Root
withdraw (tokenAllocation, proof) (Root root) = go (hashTokenAllocation tokenAllocation) unusableTokenAllocationHash proof
  where
    go :: Hash -> Hash -> Proof -> Maybe Root
    go oldSubRoot newSubRoot = \case
      [] ->
        if oldSubRoot == root
          then Just (Root newSubRoot)
          else Nothing
      Left l : q -> go (combineHash l oldSubRoot) (combineHash l newSubRoot) q
      Right r : q -> go (combineHash oldSubRoot r) (combineHash newSubRoot r) q
{-# INLINEABLE withdraw #-}

data SubScriptContext = SubScriptContext
  { subScriptContextTxInfo :: SubTxInfo
  , subScriptContextPurpose :: ScriptPurpose
  }
  deriving (Generic, Haskell.Eq, Haskell.Show)

data SubTxInfo = SubTxInfo
  { subTxInfoInputs :: [BuiltinData]
  , subTxInfoReferenceInputs :: BuiltinData
  , subTxInfoOutputs :: [BuiltinData]
  , subTgInfoFee :: BuiltinData
  , subTxInfoMint :: BuiltinData
  , subTxInfoDCert :: BuiltinData
  , subTxInfoWdrl :: BuiltinData
  , subTxInfoValidRange :: BuiltinData
  , subTxInfoSignatories :: [PubKeyHash]
  , subTxInfoRedeemers :: BuiltinData
  , subTxInfoData :: [(DatumHash, BuiltinData)]
  , subTxInfoId :: BuiltinData
  }
  deriving (Generic, Haskell.Show, Haskell.Eq)

type ScriptTxInIx = Integer

type ScriptTxOutIx = Integer

-- To speed up search and decode only the relevant part of
-- the context we accept some extra addressing hints.
type Thread = (ScriptTxInIx, ScriptTxOutIx)

type Redeemer = (Thread, [Withdrawal])

-- We carry over the original root hash.
newtype OrigRoot = OrigRoot Hash
  deriving newtype (ToData, FromData, UnsafeFromData)

type Datum = (OrigRoot, Root)

-- Invariants:
--  * Spending signature corresponding to all the accounts is present in the tx witness set.
--  * Difference in balance between the script input and the script output is equal to the amount.
--  * New root hash is present in the script output.
--
-- Non invariants:
--  * We allow multiple withdrawals in the same transaction. Users should check if they receive the expected amount.
--
-- Error codes:
--  "1" - Invalid own input.
--  "2" - Invalid withdrawal proof.
--  "3" - Missing account signature - some withdrawal realatd signature is missing.
--  "4" - Invalid input value - expecting only one token type of the distributed currency.
--  "5" - Invalid output value.
--  "6" - Invalidgoutput datum.
--  "7" - Invalid output address.
validator
  :: CurrencySymbol
  -- ^ Currency symbol of the token which we distribute.
  -> Datum
  -- ^ Datum should be a thread token name.
  -> Redeemer
  -- ^ We ignore redeemer - no need for decoding
  -> SubScriptContext
  -- ^ The script context.
  -> Bool
validator currencySymbol (origRoot, currRoot) ((txInIx, txOutIx), withdrawals) scriptContext = do
  let SubScriptContext
        { subScriptContextTxInfo = SubTxInfo{subTxInfoInputs, subTxInfoSignatories, subTxInfoOutputs, subTxInfoData}
        , subScriptContextPurpose
        } = scriptContext

      ownTxOutRef = case subScriptContextPurpose of
        Spending txOutRef -> txOutRef
        _ -> traceError "1"

      -- Check and grab the input.

      ownInput = do
        let -- We use the suggested ix to grab possibly own input.
            possiblyOwnInput@TxInInfo{txInInfoOutRef} = unsafeFromBuiltinData (subTxInfoInputs !! txInIx)
        if txInInfoOutRef == ownTxOutRef
          then possiblyOwnInput
          else traceError "1" -- Invalid own input

      -- \* Check withdrawals and compute the result.
      txSignedBy :: PubKeyHash -> Bool
      txSignedBy k = case find ((==) k) subTxInfoSignatories of
        Just _ -> True
        Nothing -> False

      (newRoot, totalTokenWithdrawn) = do
        let step (root, totalWithdrawn) withdrawal@((addr, withdrawalTokenAmount), _) = do
              case (withdraw withdrawal root, txSignedBy addr) of
                (Just root', True) -> (root', totalWithdrawn + withdrawalTokenAmount)
                (Nothing, _) -> traceError "2" -- Invalid withdrawal proof.
                (_, False) -> traceError "3" -- Missing account signature.
        foldl step (currRoot, 0) withdrawals

      -- Derive the expected outputs from the input and the results.

      TxInInfo{txInInfoResolved = TxOut{txOutAddress = expectedOwnAddress, txOutValue = inputValue}} = ownInput

      expectedOutputDatumHash = do
        let newDatum = (origRoot, newRoot)
            serializedDatum = toBuiltinData newDatum
            matchesDatum (_, builtInData) = builtInData == serializedDatum

        case find matchesDatum subTxInfoData of
          Just (datumHash, _) -> datumHash
          Nothing -> traceError "6" -- Invalid output datum
      expectedOutputValue = do
        let valuesList = AssocMap.toList $ getValue inputValue
            -- If the token bundled was drained we prevent further withdrawals.
            tokenPresent = any (\(token, _) -> token == currencySymbol) valuesList
        if not tokenPresent
          then traceError "4" -- Invalid input value
          else do
            let replaceValue orig@(token, valueMap) =
                  if token == currencySymbol
                    then case AssocMap.toList valueMap of
                      [(tokenName, value)] -> do
                        -- We don't have to check for negative value as ledger will do this for us.
                        (currencySymbol, AssocMap.fromList [(tokenName, value - totalTokenWithdrawn)])
                      _ -> traceError "4" -- Invalid input value
                    else orig
            Value $ AssocMap.fromList $ map replaceValue valuesList

  -- Check the outputs.

  case unsafeFromBuiltinData (subTxInfoOutputs !! txOutIx) of
    TxOut{txOutAddress = outputAddress, txOutValue = outputValue, txOutDatum = OutputDatumHash outputDatumHash} ->
      traceIfFalse "5" (expectedOutputValue == outputValue) -- Invalid output value
        && traceIfFalse "6" (expectedOutputDatumHash == outputDatumHash) -- Invalid output datum
        && traceIfFalse "7" (expectedOwnAddress == outputAddress) -- Invalid output address
    _ -> traceError "6" -- Invalid output datum

PlutusTx.makeLift ''SubTxInfo
PlutusTx.makeIsDataIndexed ''SubTxInfo [('SubTxInfo, 0)]

PlutusTx.makeLift ''SubScriptContext
PlutusTx.makeIsDataIndexed ''SubScriptContext [('SubScriptContext, 0)]

compileValidator :: CurrencySymbol -> PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
compileValidator currencySymbol =
  let validator' :: CurrencySymbol -> BuiltinData -> BuiltinData -> BuiltinData -> ()
      validator' currencySymbol d r p =
        check
          $ validator
            currencySymbol
            (unsafeFromBuiltinData d)
            (unsafeFromBuiltinData r)
            (unsafeFromBuiltinData p)
      errorOrApplied =
        $$(PlutusTx.compile [||validator'||])
          `PlutusTx.applyCode` PlutusTx.liftCode plcVersion100 currencySymbol
   in case errorOrApplied of
        Haskell.Left err -> Haskell.error $ "Application of role-payout validator hash to marlowe validator failed." <> err
        Haskell.Right applied -> applied

validatorBytes :: CurrencySymbol -> SerialisedScript
validatorBytes currencySymbol = serialiseCompiledCode (compileValidator currencySymbol)

hashScript :: PlutusTx.CompiledCode fn -> ScriptHash
hashScript =
  ScriptHash
    . toBuiltin
    . (Hash.hashToBytes :: Hash.Hash Hash.Blake2b_224 SBS.ShortByteString -> BS.ByteString)
    . Hash.hashWith (BS.append "\x02" . SBS.fromShort) -- For Plutus V2.
    . serialiseCompiledCode

validatorHash :: CurrencySymbol -> ScriptHash
validatorHash currencySymbol = hashScript (compileValidator currencySymbol)
