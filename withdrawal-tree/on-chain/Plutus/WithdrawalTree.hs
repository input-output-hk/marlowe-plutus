{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

-- | Marlowe semantics validator.
module Plutus.WithdrawalTree (
  Hash,
  Root,
  Proof,
  withdraw,
  validator,
) where

import GHC.Generics (Generic)
import PlutusLedgerApi.V1 (ToData)
import PlutusLedgerApi.V1.Scripts (DatumHash)
import PlutusLedgerApi.V2 (
  CurrencySymbol,
  OutputDatum (OutputDatumHash),
  PubKeyHash (..),
  TxInInfo (TxInInfo, txInInfoResolved),
  TxOut (TxOut, txOutAddress, txOutDatum, txOutValue),
  UnsafeFromData (unsafeFromBuiltinData),
  Value (Value, getValue),
  toBuiltinData,
 )
import PlutusLedgerApi.V2.Contexts (ScriptPurpose (..), txInInfoOutRef)
import qualified PlutusTx.AssocMap as AssocMap
import PlutusTx.Builtins.Class (stringToBuiltinByteString)
import PlutusTx.Prelude
import Prelude (Show (show))
import qualified Prelude as Haskell

type Hash = BuiltinByteString

type Proof = [Either Hash Hash]

type Account = (PubKeyHash, Integer)

hash :: BuiltinByteString -> Hash
hash = sha2_256
{-# INLINEABLE hash #-}

hashAccount :: Account -> Hash
hashAccount (PubKeyHash addr, amount) = hash $ addr `appendByteString` "#" `appendByteString` (stringToBuiltinByteString $ show amount)
{-# INLINEABLE hashAccount #-}

combineHash :: Hash -> Hash -> Hash
combineHash h h' = hash (appendByteString h h')
{-# INLINEABLE combineHash #-}

newtype Root = Root Hash
  deriving newtype (ToData)

type Withdrawal = (Account, Proof)

withdraw :: Withdrawal -> Root -> Maybe Root
withdraw (account@(addr, _), proof) (Root root) = go (hashAccount account) (hashAccount account') proof
  where
    account' = (addr, 0)
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
  { subTxInfoInputs :: [BuiltinData] -- [TxInInfo]
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
  deriving newtype (ToData)

type Datum = (OrigRoot, Root)

-- Invariants:
--  * Spending signature corresponding to all the accounts is present in the tx witness set.
--  * Difference in balance between the script input and the script output is equal to the amount.
--  * New root hash is present in the script output.
--
-- Non invariants:
--  * We allow multiple withdrawals in the same transaction. Users should check if they receive the expected amount.
--  * We don't check if the input contains the tokens at all.
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

      -- \* Check and grab the input.

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

      -- \* Derive the expected outputs from the input and the results.

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

  -- \* Check the outputs.

  case unsafeFromBuiltinData (subTxInfoOutputs !! txOutIx) of
    TxOut{txOutAddress = outputAddress, txOutValue = outputValue, txOutDatum = OutputDatumHash outputDatumHash} ->
      traceIfFalse "5" (expectedOutputValue == outputValue) -- Invalid output value
        && traceIfFalse "6" (expectedOutputDatumHash == outputDatumHash) -- Invalid output datum
        && traceIfFalse "7" (expectedOwnAddress == outputAddress) -- Invalid output address
    _ -> traceError "6" -- Invalid output datum
