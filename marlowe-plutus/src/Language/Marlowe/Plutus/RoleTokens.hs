{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}

module Language.Marlowe.Plutus.RoleTokens (
  MintAction (..),
  RoleTokens,
  mkRoleTokens,
  mkRoleTokensHash,
  policy,
) where

import Language.Marlowe.Plutus.RoleTokens.Types (
  MintAction (..),
  RoleTokens,
  RoleTokensHash,
  mkRoleTokens,
  mkRoleTokensHash,
 )
import qualified Plutus.V2.Ledger.Api as PV2
import qualified Plutus.V2.Ledger.Contexts as PV2
import PlutusTx (CompiledCode)
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AssocMap
import PlutusTx.Prelude

-- | One shot policy which encodes the `txOutRef` and tokens minting in its hash.
-- In theory this currency can be reused across multiple Marlowe Contracts when the precise
-- token distribution is not so important to the participant but rather a possible token
-- redundancy or uniqueness.
mkPolicy
  :: RoleTokensHash
  -> PV2.TxOutRef
  -> MintAction
  -> PV2.ScriptContext
  -> Bool
mkPolicy roleTokensHash seedInput action context =
  case action of
    Mint -> validateMinting roleTokensHash seedInput context
    Burn -> validateBurning context

{-# INLINEABLE validateMinting #-}
validateMinting :: RoleTokensHash -> PV2.TxOutRef -> PV2.ScriptContext -> Bool
validateMinting roleTokens seedInput context =
  traceIfFalse "Mint failed"
    $ seedInputIsConsumed
    && roleTokensAreMinted
  where
    PV2.ScriptContext{scriptContextTxInfo = txInfo} = context
    ownCurrency = PV2.ownCurrencySymbol context

    seedInputIsConsumed = do
      let PV2.TxOutRef txId txIx = seedInput
      PV2.spendsOutput txInfo txId txIx

    roleTokensAreMinted :: Bool
    roleTokensAreMinted = txMintedRoleTokens == roleTokens
      where
        txMintedRoleTokens = do
          let tokensMap = fromMaybe AssocMap.empty . AssocMap.lookup ownCurrency . PV2.getValue . PV2.txInfoMint $ txInfo
          mkRoleTokensHash . mkRoleTokens . AssocMap.toList $ tokensMap

{-# INLINEABLE validateBurning #-}
validateBurning :: PV2.ScriptContext -> Bool
validateBurning context = do
  traceIfFalse "Burn failed" allBurned
  where
    ownCurrency = PV2.ownCurrencySymbol context
    -- Allow only burning here
    allBurned = missingFromOutputsValue ownCurrency context

{-# INLINEABLE missingFromOutputsValue #-}
missingFromOutputsValue :: PV2.CurrencySymbol -> PV2.ScriptContext -> Bool
missingFromOutputsValue currencySymbol PV2.ScriptContext{scriptContextTxInfo} = do
  let PV2.TxInfo{txInfoOutputs} = scriptContextTxInfo
      missingFromOutputValue PV2.TxOut{txOutValue} = isNothing . AssocMap.lookup currencySymbol . PV2.getValue $ txOutValue
  all missingFromOutputValue txInfoOutputs

policy :: RoleTokens -> PV2.TxOutRef -> CompiledCode (BuiltinData -> BuiltinData -> ())
policy roleTokens txOutRef = do
  let roleTokensHash = mkRoleTokensHash roleTokens
  $$(PlutusTx.compile [||\rs seed -> wrapMintingPolicy (mkPolicy rs seed)||])
    `PlutusTx.applyCode` PlutusTx.liftCode roleTokensHash
    `PlutusTx.applyCode` PlutusTx.liftCode txOutRef

-- Extracted from plutus-ledger

-- | Signature of an untyped minting policy script.
type MintingPolicyFn = BuiltinData -> BuiltinData -> ()

-- | Turns typed function into a minting policy which can be used
-- on the chain.
{-# INLINEABLE wrapMintingPolicy #-}
wrapMintingPolicy
  :: (PV2.UnsafeFromData redeemer, PV2.UnsafeFromData context)
  => (redeemer -> context -> Bool)
  -> MintingPolicyFn
wrapMintingPolicy f r c =
  PlutusTx.Prelude.check (f redeemer context)
  where
    redeemer = PV2.unsafeFromBuiltinData r
    context = PV2.unsafeFromBuiltinData c
