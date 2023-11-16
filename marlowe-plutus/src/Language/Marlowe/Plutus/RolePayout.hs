{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
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

-- | Marlowe role-payout validator.
module Language.Marlowe.Plutus.RolePayout (
  -- * Validation
  rolePayoutValidator,
  rolePayoutValidatorBytes,
  rolePayoutValidatorHash,
) where

import Language.Marlowe.Core.V1.Semantics.Types as Semantics
import Language.Marlowe.Plutus (hashScript)
import PlutusLedgerApi.V1.Value qualified as Val
import PlutusLedgerApi.V2 (
  ScriptContext (scriptContextTxInfo),
  ScriptHash (..),
  SerialisedScript,
  UnsafeFromData (..),
  serialiseCompiledCode,
 )
import PlutusLedgerApi.V2.Contexts (valueSpent)
import PlutusTx (CompiledCode)
import PlutusTx qualified
import PlutusTx.Plugin ()
import PlutusTx.Prelude as PlutusTxPrelude hiding (traceError, traceIfFalse)

-- Conditionally suppress traces, in order to save bytes.

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

{-# INLINEABLE rolePayoutValidator #-}

-- | The Marlowe payout validator.
rolePayoutValidator :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
rolePayoutValidator =
  $$(PlutusTx.compile [||rolePayoutValidator'||])
  where
    rolePayoutValidator' :: BuiltinData -> BuiltinData -> BuiltinData -> ()
    rolePayoutValidator' d r p =
      check
        $ mkRolePayoutValidator
          (unsafeFromBuiltinData d)
          (unsafeFromBuiltinData r)
          (unsafeFromBuiltinData p)

-- | The Marlowe payout validator.
mkRolePayoutValidator
  :: (CurrencySymbol, TokenName)
  -- ^ The datum is the currency symbol and role name for the payout.
  -> ()
  -- ^ No redeemer is required.
  -> ScriptContext
  -- ^ The script context.
  -> Bool
  -- ^ Whether the transaction validated.
mkRolePayoutValidator (currency, role) _ ctx =
  -- The role token for the correct currency must be present.
  -- [Marlowe-Cardano Specification: "17. Payment authorized".]
  Val.singleton currency role 1 `Val.leq` valueSpent (scriptContextTxInfo ctx)

-- | The hash of the Marlowe payout validator.
rolePayoutValidatorHash :: ScriptHash
rolePayoutValidatorHash = hashScript rolePayoutValidator

-- | The serialisation of the Marlowe payout validator.
rolePayoutValidatorBytes :: SerialisedScript
rolePayoutValidatorBytes = serialiseCompiledCode rolePayoutValidator
