{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}

-- | Marlowe validators.
module Language.Marlowe.Plutus (
  -- * Utilities
  hashScript,
) where

import Cardano.Crypto.Hash qualified as Hash
import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as SBS
import PlutusLedgerApi.V2 (
  ScriptHash (..),
  serialiseCompiledCode,
 )
import PlutusTx (CompiledCode)
import PlutusTx.Prelude as PlutusTxPrelude

-- | Compute the hash of a script.
hashScript :: CompiledCode fn -> ScriptHash
hashScript =
  ScriptHash
    . toBuiltin
    . (Hash.hashToBytes :: Hash.Hash Hash.Blake2b_224 SBS.ShortByteString -> BS.ByteString)
    . Hash.hashWith (BS.append "\x02" . SBS.fromShort) -- For Plutus V2.
    . serialiseCompiledCode
