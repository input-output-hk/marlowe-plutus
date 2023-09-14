-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-----------------------------------------------------------------------------
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

module Language.Marlowe.Scripts.Charli3 (
  mkValidator,
  validator,
  validatorBytes,
  validatorHash,
) where

import Language.Marlowe.Core.V1.Semantics.Types qualified as V1
import Language.Marlowe.Plutus (hashScript, serialiseCompiledCode)
import Language.Marlowe.Scripts qualified as V1.Scripts
import OracleFeed qualified as C3
import Plutus.V1.Ledger.Address (scriptHashAddress)
import Plutus.V1.Ledger.Value (adaSymbol, getValue, valueOf)
import Plutus.V2.Ledger.Api (
  OutputDatum (OutputDatum),
  Redeemer (..),
  ScriptContext (..),
  ScriptPurpose (Spending),
  SerializedScript,
  TxInInfo (TxInInfo, txInInfoOutRef, txInInfoResolved),
  TxInfo (..),
  TxOut (..),
  ValidatorHash,
  fromBuiltinData,
 )
import Plutus.V2.Ledger.Api qualified as PV2
import PlutusTx (CompiledCode)
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Prelude as PlutusTxPrelude hiding (traceError, traceIfFalse)

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

mkValidator
  :: ValidatorHash
  -- ^ The hash of the corresponding Marlowe validator.
  -> V1.CurrencySymbol
  -- ^ The policy ID for the Charli3 oracle feed.
  -> V1.TokenName
  -- ^ The token name from the Charli3 oracle feed.
  -> V1.ChoiceName
  -- ^ Name of the oracle choice in Marlowe.
  -> V1.TokenName
  -- ^ Datum should be a thread token name.
  -> BuiltinData
  -- ^ Ignored.
  -> ScriptContext
  -- ^ The script context.
  -> Bool
mkValidator
  marloweValidatorHash
  charli3CurrencySymbol
  charli3TokenName
  charli3ChoiceName
  threadTokenName
  _
  ScriptContext{scriptContextTxInfo = TxInfo{..}, scriptContextPurpose} = do
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
            (\TxInInfo{txInInfoResolved = TxOut{txOutValue}} -> valueOf txOutValue charli3CurrencySymbol charli3TokenName > 1)
            txInfoReferenceInputs of
            [TxInInfo{txInInfoResolved = TxOut{txOutDatum = OutputDatum datum}}] -> datum
            _ -> traceError "C3d" -- Charli3 oracle feed not found.
        charli3Oracle =
          case C3.getPriceDatas . C3.OracleFeed $ charli3Datum of
            [pd] -> C3.getPrice . C3.getPriceMap $ pd
            _ -> traceError "C3e" -- Precisely one Charli3 price value not found.
        (currencySymbol, roleName) = do
          let valuesList = AssocMap.toList $ getValue ownValue
          case valuesList of
            [(possibleAdaSymbol, _), (currencySymbol, AssocMap.toList -> [(roleName, _)])]
              | possibleAdaSymbol == adaSymbol -> (currencySymbol, roleName)
            [(currencySymbol, AssocMap.toList -> [(roleName, _)]), _] -> (currencySymbol, roleName)
            _ -> traceError "C3f" -- Precisely one role token not found.
        marloweValidatorAddress = scriptHashAddress marloweValidatorHash
        marloweInput = case find (\TxInInfo{txInInfoResolved} -> txOutAddress txInInfoResolved == marloweValidatorAddress) txInfoInputs of
          Just input -> input
          Nothing -> traceError "C3g" -- Marlowe script UTXO not found.
        threadTokenOk = do
          let marloweValue = txOutValue $ txInInfoResolved marloweInput
          traceIfFalse "C3h" (valueOf marloweValue currencySymbol threadTokenName > 0) -- Marlowe thread token not found.
        marloweRedeemerOk = do
          let TxInInfo{txInInfoOutRef = marloweTxOutRef} = marloweInput
              inputContentUsesRole (V1.IChoice (V1.ChoiceId choiceId (V1.Role role)) choiceNum) = choiceId == charli3ChoiceName && role == roleName && choiceNum == charli3Oracle
              inputContentUsesRole _ = False
              inputUsesRole (V1.Scripts.MerkleizedTxInput inputContent _) = inputContentUsesRole inputContent
              inputUsesRole (V1.Scripts.Input inputContent) = inputContentUsesRole inputContent
              inputs :: V1.Scripts.MarloweInput
              inputs = case AssocMap.lookup (Spending marloweTxOutRef) txInfoRedeemers of
                Nothing -> traceError "C3i" -- Marlowe redeemer not found.
                Just (Redeemer bytes) -> case fromBuiltinData bytes of
                  Just inputs -> inputs
                  Nothing -> traceError "C3j" -- Marlowe redeemer not decoded.
          isJust $ find inputUsesRole inputs

    ownOutputOk && threadTokenOk && marloweRedeemerOk

validator :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
validator = do
  -- FIXME: Hard-coded values used for proof-of-concept demonstration only.
  let charli3CurrencySymbol = "adb6f28429c860672e877f15f505a39899c1cdef9c68b2081a172736"
      charli3TokenName = "OracleFeed"
      charli3ChoiceName = "Charli3 ADAUSD"
  let validator'
        :: ValidatorHash -> V1.CurrencySymbol -> V1.TokenName -> V1.ChoiceName -> BuiltinData -> BuiltinData -> BuiltinData -> ()
      validator' mvh c3p c3n c3c d r p = PlutusTxPrelude.check $ mkValidator mvh c3p c3n c3c (PV2.unsafeFromBuiltinData d) r (PV2.unsafeFromBuiltinData p)
  $$(PlutusTx.compile [||validator'||])
    `PlutusTx.applyCode` PlutusTx.liftCode V1.Scripts.marloweValidatorHash
    `PlutusTx.applyCode` PlutusTx.liftCode charli3CurrencySymbol
    `PlutusTx.applyCode` PlutusTx.liftCode charli3TokenName
    `PlutusTx.applyCode` PlutusTx.liftCode charli3ChoiceName

validatorBytes :: SerializedScript
validatorBytes = serialiseCompiledCode validator

validatorHash :: ValidatorHash
validatorHash = hashScript validator
