{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}

-- | Plutus validator for Charli3 bridge to Marlowe contracts.
module Language.Marlowe.Plutus.Charli3 (
  -- * Validator
  mkValidator,
  validator,
  validatorBytes,
  validatorHash,
) where

import Language.Marlowe.Core.V1.Semantics.Types qualified as V1
import Language.Marlowe.Plutus (hashScript)
import Language.Marlowe.Plutus.Semantics (marloweValidatorHash)
import Language.Marlowe.Scripts.Types qualified as V1.Scripts
import OracleFeed qualified as C3
import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Value (adaSymbol, getValue, valueOf)
import PlutusLedgerApi.V2 (
  OutputDatum (OutputDatum, OutputDatumHash),
  Redeemer (..),
  ScriptContext (..),
  ScriptHash,
  ScriptPurpose (Spending),
  SerialisedScript,
  TxInInfo (TxInInfo, txInInfoOutRef, txInInfoResolved),
  TxInfo (..),
  TxOut (..),
  fromBuiltinData,
  serialiseCompiledCode,
 )
import PlutusLedgerApi.V2 qualified as PV2
import PlutusLedgerApi.V2.Contexts (findDatum, txSignedBy)
import PlutusTx (CompiledCode)
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Prelude as PlutusTxPrelude hiding (traceError, traceIfFalse)
import Prelude qualified as Haskell (Either (..), error, show)

#ifdef TRACE_PLUTUS

import PlutusTx.Prelude (traceError, traceIfFalse)

#else

{-# INLINABLE traceError #-}
traceError :: BuiltinString -> a
traceError _ = error ()

{-# INLINABLE traceIfFalse #-}
traceIfFalse :: BuiltinString -> a -> a
traceIfFalse _ = id

#endif

-- | Create the Plutus validator for the Charli3 bridge.
mkValidator
  :: ScriptHash
  -- ^ The hash of the corresponding Marlowe validator.
  -> V1.CurrencySymbol
  -- ^ The policy ID for the Charli3 oracle feed.
  -> V1.TokenName
  -- ^ The token name from the Charli3 oracle feed.
  -> V1.ChoiceName
  -- ^ Name of the oracle choice in Marlowe.
  -> (PV2.PubKeyHash, V1.TokenName)
  -- ^ Datum should be the public key hash of the oracle operator and a thread token name.
  -> Bool
  -- ^ The redeemer should be a boolean indicating whether the script continues running.
  -> ScriptContext
  -- ^ The script context.
  -> Bool
mkValidator _ _ _ _ (pkh, _) False ScriptContext{scriptContextTxInfo = txInfo} = txSignedBy txInfo pkh
mkValidator
  marloweValidatorHash
  charli3CurrencySymbol
  charli3TokenName
  charli3ChoiceName
  (_, threadTokenName)
  True
  ScriptContext{scriptContextTxInfo = txInfo@TxInfo{..}, scriptContextPurpose} = do
    let ownInput =
          case scriptContextPurpose of
            Spending txOutRef ->
              case find (\TxInInfo{txInInfoOutRef} -> txInInfoOutRef == txOutRef) txInfoInputs of
                Just input -> input
                Nothing -> traceError "C3a" -- Impossible, script input not found.
            _ -> traceError "C3b" -- Not a spending script.
        TxInInfo{txInInfoResolved = TxOut{txOutAddress = ownAddress, txOutValue = ownValue, txOutDatum = ownDatum}} = ownInput

        ownOutputOk =
          case filter (\TxOut{txOutAddress} -> ownAddress == txOutAddress) txInfoOutputs of
            [TxOut{txOutValue, txOutDatum}] -> txOutValue == ownValue && txOutDatum == ownDatum
            _ -> traceError "C3c" -- Script value or datum changed.
        PV2.Datum charli3Datum =
          case filter
            (\TxInInfo{txInInfoResolved = TxOut{txOutValue}} -> valueOf txOutValue charli3CurrencySymbol charli3TokenName > 0)
            txInfoReferenceInputs of
            [TxInInfo{txInInfoResolved = TxOut{txOutDatum = OutputDatum datum}}] -> datum
            [TxInInfo{txInInfoResolved = TxOut{txOutDatum = OutputDatumHash hash}}] ->
              case findDatum hash txInfo of
                Just datum -> datum
                Nothing -> traceError "C3d" -- Charli3 datum not found.
            _ -> traceError "C3e" -- Charli3 oracle feed not found.
        charli3Oracle =
          case C3.getPriceDatas . C3.OracleFeed $ charli3Datum of
            [pd] -> C3.getPrice . C3.getPriceMap $ pd
            _ -> traceError "C3f" -- Precisely one Charli3 price value not found.
        (currencySymbol, roleName) = do
          let valuesList = AssocMap.toList $ getValue ownValue
          case valuesList of
            [(possibleAdaSymbol, _), (currencySymbol, AssocMap.toList -> [(roleName, _)])]
              | possibleAdaSymbol == adaSymbol -> (currencySymbol, roleName)
            [(currencySymbol, AssocMap.toList -> [(roleName, _)]), _] -> (currencySymbol, roleName)
            _ -> traceError "C3g" -- Precisely one role token not found.
        marloweValidatorAddress = scriptHashAddress marloweValidatorHash
        marloweInput = case find (\TxInInfo{txInInfoResolved} -> txOutAddress txInInfoResolved == marloweValidatorAddress) txInfoInputs of
          Just input -> input
          Nothing -> traceError "C3h" -- Marlowe script UTXO not found.
        threadTokenOk = do
          let marloweValue = txOutValue $ txInInfoResolved marloweInput
          traceIfFalse "C3i" (valueOf marloweValue currencySymbol threadTokenName > 0) -- Marlowe thread token not found.
        marloweRedeemerOk = do
          let TxInInfo{txInInfoOutRef = marloweTxOutRef} = marloweInput
              -- Logically equivalent to "choice number matches if choice name matches and role matches".
              inputContentUsesRole (V1.IChoice (V1.ChoiceId choiceId (V1.Role role)) choiceNum) =
                choiceId /= charli3ChoiceName || role /= roleName || choiceNum == charli3Oracle
              inputContentUsesRole _ = True
              inputUsesRole (V1.Scripts.MerkleizedTxInput inputContent _) = inputContentUsesRole inputContent
              inputUsesRole (V1.Scripts.Input inputContent) = inputContentUsesRole inputContent
              inputs :: V1.Scripts.MarloweInput
              inputs = case AssocMap.lookup (Spending marloweTxOutRef) txInfoRedeemers of
                Nothing -> traceError "C3j" -- Marlowe redeemer not found.
                Just (Redeemer bytes) -> case fromBuiltinData bytes of
                  Just inputs -> inputs
                  Nothing -> traceError "C3k" -- Marlowe redeemer not decoded.
          all inputUsesRole inputs

    ownOutputOk && threadTokenOk && marloweRedeemerOk

-- | The Plutus validator for the Charli3 bridge.
validator
  :: V1.CurrencySymbol
  -- ^ The policy ID for the Charli3 oracle feed.
  -> V1.TokenName
  -- ^ The token name from the Charli3 oracle feed.
  -> V1.ChoiceName
  -- The validator.
  -> CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
validator charli3CurrencySymbol charli3TokenName charli3ChoiceName = do
  -- FIXME: Hard-coded values used for proof-of-concept demonstration only.
  let validator'
        :: ScriptHash -> V1.CurrencySymbol -> V1.TokenName -> V1.ChoiceName -> BuiltinData -> BuiltinData -> BuiltinData -> ()
      validator' mvh c3p c3n c3c d r p =
        PlutusTxPrelude.check
          $ mkValidator mvh c3p c3n c3c (PV2.unsafeFromBuiltinData d) (PV2.unsafeFromBuiltinData r) (PV2.unsafeFromBuiltinData p)
      errorOrApplied =
        $$(PlutusTx.compile [||validator'||])
          `PlutusTx.applyCode` PlutusTx.liftCode plcVersion100 marloweValidatorHash
          >>= (`PlutusTx.applyCode` PlutusTx.liftCode plcVersion100 charli3CurrencySymbol)
          >>= (`PlutusTx.applyCode` PlutusTx.liftCode plcVersion100 charli3TokenName)
          >>= (`PlutusTx.applyCode` PlutusTx.liftCode plcVersion100 charli3ChoiceName)
   in case errorOrApplied of
        Haskell.Left err -> Haskell.error $ "Application of marlowe validator hash to openRole validator failed." <> Haskell.show err
        Haskell.Right applied -> applied

-- | Compute the bytes of the Plutus validator for the Charli3 bridge.
validatorBytes
  :: V1.CurrencySymbol
  -- ^ The policy ID for the Charli3 oracle feed.
  -> V1.TokenName
  -- ^ The token name from the Charli3 oracle feed.
  -> V1.ChoiceName
  -- ^ Name of the oracle choice in Marlowe.
  -> SerialisedScript
  -- ^ The serialized Plutus script.
validatorBytes = ((serialiseCompiledCode .) .) . validator

-- | Compute the hash of the Plutus validator for the Charli3 bridge.
validatorHash
  :: V1.CurrencySymbol
  -- ^ The policy ID for the Charli3 oracle feed.
  -> V1.TokenName
  -- ^ The token name from the Charli3 oracle feed.
  -> V1.ChoiceName
  -- ^ Name of the oracle choice in Marlowe.
  -> ScriptHash
  -- ^ The validator hash.
validatorHash = ((hashScript .) .) . validator
