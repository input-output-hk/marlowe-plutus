{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

-- | Marlowe open-roles validator.
module Language.Marlowe.Plutus.OpenRoles (
  -- * Validation
  openRoleValidator,
  openRoleValidatorBytes,
  openRoleValidatorHash,
) where

import GHC.Generics (Generic)
import Language.Marlowe.Plutus (hashScript)
import Language.Marlowe.Plutus.Script (marloweValidatorHash)
import Language.Marlowe.Plutus.ScriptTypes (MarloweInput, MarloweTxInput (..))
import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V1.Value (valueOf)
import PlutusLedgerApi.V2 (
  Address (..),
  Credential (..),
  FromData (..),
  Redeemer (..),
  ScriptHash (..),
  ScriptPurpose (Spending),
  SerialisedScript,
  TxInInfo (TxInInfo, txInInfoOutRef, txInInfoResolved),
  UnsafeFromData (..),
  Value (..),
  adaSymbol,
  serialiseCompiledCode,
 )
import PlutusLedgerApi.V2.Tx (TxOut (TxOut, txOutAddress, txOutValue))
import PlutusTx (CompiledCode)
import PlutusTx.Plugin ()

import Language.Marlowe.Plutus.Semantics.Types as Semantics (
  ChoiceId (ChoiceId),
  InputContent (IChoice, IDeposit),
  Party (Role),
  TokenName,
 )
import PlutusTx.Prelude as PlutusTxPrelude (
  Bool (False),
  BuiltinData,
  Eq (..),
  Maybe (Just, Nothing),
  Ord ((>)),
  Semigroup ((<>)),
  check,
  find,
  isJust,
  traceError,
  traceIfFalse,
  ($),
  (&&),
 )

import qualified PlutusTx
import qualified PlutusTx.AssocMap as AssocMap
import qualified Prelude as Haskell

-- By decoding only the part of the script context I was able
-- to bring down the size of the validator from 4928 to 4540 bytes.
data SubScriptContext = SubScriptContext
  { subScriptContextTxInfo :: SubTxInfo
  , subScriptContextPurpose :: ScriptPurpose
  }
  deriving (Generic, Haskell.Eq, Haskell.Show)

instance Eq SubScriptContext where
  {-# INLINEABLE (==) #-}
  SubScriptContext info purpose == SubScriptContext info' purpose' = info == info' && purpose == purpose'

data SubTxInfo = SubTxInfo
  { subTxInfoInputs :: [TxInInfo]
  , subTxInfoReferenceInputs :: BuiltinData
  , subTxInfoOutputs :: BuiltinData
  , subTxInfoFee :: BuiltinData
  , subTxInfoMint :: BuiltinData
  , subTxInfoDCert :: BuiltinData
  , subTxInfoWdrl :: BuiltinData
  , subTxInfoValidRange :: BuiltinData
  , subTxInfoSignatories :: BuiltinData
  , subTxInfoRedeemers :: AssocMap.Map ScriptPurpose Redeemer
  , subTxInfoData :: BuiltinData
  , subTxInfoId :: BuiltinData
  }
  deriving (Generic, Haskell.Show, Haskell.Eq)

instance Eq SubTxInfo where
  {-# INLINEABLE (==) #-}
  SubTxInfo i ri o f m c w r s rs d tid == SubTxInfo i' ri' o' f' m' c' w' r' s' rs' d' tid' =
    i
      == i'
      && ri
      == ri'
      && o
      == o'
      && f
      == f'
      && m
      == m'
      && c
      == c'
      && w
      == w'
      && r
      == r'
      && s
      == s'
      && rs
      == rs'
      && d
      == d'
      && tid
      == tid'

{- Open Role validator - it releases role token(s) (you can put more coins of the same role token to it) based on a few conditions:

   1. Value should contain only min. ada and a specific role token(s).
   2. Transaction should spend Marlowe output which contains corresponding thread token (the same currency symbol as
   role and token name as defined in the inlined datum).
   3. The list of Marlowe inputs should contain at least one action either `IDeposit` or `IChoice` dedicated for the same role
   party as the locked role token.

   In the current version we assume that thread token has the same currency as the role token, even though this might actually be undesired.

   Error codes:

   "1" - Own input (never happen) or Marlowe input not found.
   "2" - Invalid own value - we expect only the role token(s) and min ADA.
   "3" - Invalid Marlowe redeemer.
   "4" - Missing thread token.
-}
mkOpenRoleValidator
  :: ScriptHash
  -- ^ The hash of the corresponding Marlowe validator.
  -> Semantics.TokenName
  -- ^ Datum should be a thread token name.
  -> BuiltinData
  -- ^ We ignore redeemer - no need for decoding
  -> SubScriptContext
  -- ^ The script context.
  -> Bool
mkOpenRoleValidator
  marloweValidatorHash
  threadTokenName
  _
  SubScriptContext
    { subScriptContextTxInfo = SubTxInfo{subTxInfoInputs, subTxInfoRedeemers}
    , subScriptContextPurpose = Spending txOutRef
    } = do
    let -- Performance:
        -- In the case of three inputs `find` seems to be faster than custom single pass over the list.
        -- Inlined pattern matching over `Maybe` in both cases also seems to be faster than separate helper function.
        ownInput = case find (\TxInInfo{txInInfoOutRef} -> txInInfoOutRef == txOutRef) subTxInfoInputs of
          Just input -> input
          Nothing -> traceError "1" -- Own input not found.
        marloweInput = case find
          ( \TxInInfo{txInInfoResolved} -> addressCredential (txOutAddress txInInfoResolved) == ScriptCredential marloweValidatorHash
          )
          subTxInfoInputs of
          Just input -> input
          Nothing -> traceError "1" -- Marlowe input not found.
        TxInInfo{txInInfoResolved = TxOut{txOutValue = ownValue}} = ownInput

        -- Extract role token information from the own input `Value`.
        (currencySymbol, roleName) = do
          let valuesList = AssocMap.toList $ getValue ownValue
          -- Value should contain only min. ADA and a specific role token(s) (we can have few coins of the same role
          -- token - they are all released).
          -- Performance: `find` performs here clearly worse.
          case valuesList of
            [(possibleAdaSymbol, _), (currencySymbol, AssocMap.toList -> [(roleName, _)])]
              | possibleAdaSymbol PlutusTxPrelude.== adaSymbol -> (currencySymbol, roleName)
            [(currencySymbol, AssocMap.toList -> [(roleName, _)]), _] -> (currencySymbol, roleName)
            _ -> traceError "2" -- Invalid value - we expect only the role token(s).

        -- In order to release the role token we have to encounter an action which uses/unlocks the role.
        -- All the other actions will be checked by Marlowe validator itself.
        marloweRedeemerOk = do
          let TxInInfo{txInInfoOutRef = marloweTxOutRef} = marloweInput
              inputContentUsesRole (Semantics.IDeposit _ (Semantics.Role role) _ _) = role PlutusTxPrelude.== roleName
              inputContentUsesRole (Semantics.IChoice (Semantics.ChoiceId _ (Semantics.Role role)) _) = role PlutusTxPrelude.== roleName
              inputContentUsesRole _ = False

              inputUsesRole (MerkleizedTxInput inputContent _) = inputContentUsesRole inputContent
              inputUsesRole (Input inputContent) = inputContentUsesRole inputContent

              inputs :: MarloweInput
              inputs = case AssocMap.lookup (Spending marloweTxOutRef) subTxInfoRedeemers of
                Nothing -> traceError "3" -- Invalid Marlowe redeemer
                Just (Redeemer bytes) -> case fromBuiltinData bytes of
                  Just inputs -> inputs
                  _ -> traceError "3" -- Invalid Marlowe redeemer
          isJust $ find inputUsesRole inputs

        -- Check the Marlowe input `Value` for the thread token.
        threadTokenOk = do
          let marloweValue = txOutValue $ txInInfoResolved marloweInput
          traceIfFalse "4" (valueOf marloweValue currencySymbol threadTokenName > 0)
    marloweRedeemerOk && threadTokenOk
mkOpenRoleValidator _ _ _ _ = False

PlutusTx.makeLift ''SubTxInfo
PlutusTx.makeIsDataIndexed ''SubTxInfo [('SubTxInfo, 0)]

PlutusTx.makeLift ''SubScriptContext
PlutusTx.makeIsDataIndexed ''SubScriptContext [('SubScriptContext, 0)]

-- Copied from marlowe-cardano. This is pretty standard way to minimize size of the typed validator:
--  * Wrap validator function so it accepts raw `BuiltinData`.
--  * Create a validator which is simply typed.
--  * Create "typed by `Any` validator".
--  * Coerce it if you like. This step is not required - we only need `TypedValidator`.
openRoleValidator :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
openRoleValidator =
  let openRoleValidator' :: ScriptHash -> BuiltinData -> BuiltinData -> BuiltinData -> ()
      openRoleValidator' mvh d r p =
        check
          $ mkOpenRoleValidator
            mvh
            (unsafeFromBuiltinData d)
            (unsafeFromBuiltinData r)
            (unsafeFromBuiltinData p)
      errorOrApplied =
        $$(PlutusTx.compile [||openRoleValidator'||])
          `PlutusTx.applyCode` PlutusTx.liftCode plcVersion100 marloweValidatorHash
   in case errorOrApplied of
        Haskell.Left err -> Haskell.error $ "Application of marlowe validator hash to openRole validator failed." <> err
        Haskell.Right applied -> applied

openRoleValidatorBytes :: SerialisedScript
openRoleValidatorBytes = serialiseCompiledCode openRoleValidator

openRoleValidatorHash :: ScriptHash
openRoleValidatorHash = hashScript openRoleValidator
