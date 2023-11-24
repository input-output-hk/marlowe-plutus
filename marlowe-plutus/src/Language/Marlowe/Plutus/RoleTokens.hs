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
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module Language.Marlowe.Plutus.RoleTokens (
  -- * Minting
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
import PlutusCore.Version (plcVersion100)
import PlutusTx (CompiledCode)
import PlutusTx.Prelude (
  Bool,
  BuiltinData,
  Eq ((==)),
  Semigroup ((<>)),
  all,
  check,
  fromMaybe,
  isNothing,
  traceIfFalse,
  ($),
  (&&),
  (.),
  (>>=),
 )

import qualified PlutusLedgerApi.V2 as PV2
import qualified PlutusLedgerApi.V2.Contexts as PV2
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AssocMap
import qualified Prelude as Haskell

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
policy roleTokens txOutRef =
  let roleTokensHash = mkRoleTokensHash roleTokens
      errorOrApplied =
        $$(PlutusTx.compile [||\rs seed -> wrapMintingPolicy (mkPolicy rs seed)||])
          `PlutusTx.applyCode` PlutusTx.liftCode plcVersion100 roleTokensHash
          >>= (`PlutusTx.applyCode` PlutusTx.liftCode plcVersion100 txOutRef)
   in case errorOrApplied of
        Haskell.Left err -> Haskell.error $ "Application of arguments to minting validator failed." <> err
        Haskell.Right applied -> applied

-- Extracted from plutus-ledger

-- | Signature of an untyped minting policy script.
type MintingPolicyFn = BuiltinData -> BuiltinData -> ()

{-# INLINEABLE wrapMintingPolicy #-}

-- | Turns typed function into a minting policy which can be used
-- on the chain.
wrapMintingPolicy
  :: (PV2.UnsafeFromData redeemer, PV2.UnsafeFromData context)
  => (redeemer -> context -> Bool)
  -> MintingPolicyFn
wrapMintingPolicy f r c =
  PlutusTx.Prelude.check (f redeemer context)
  where
    redeemer = PV2.unsafeFromBuiltinData r
    context = PV2.unsafeFromBuiltinData c
