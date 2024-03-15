{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
) where

import PlutusTx.Builtins.Class (stringToBuiltinByteString)
import PlutusTx.Prelude
import Prelude (Show (show))

type Hash = BuiltinByteString

type Proof = [Either Hash Hash]

type Address = BuiltinByteString

type Account = (Address, Integer)

hash :: BuiltinByteString -> Hash
hash = sha2_256
{-# INLINEABLE hash #-}

hashAccount :: Account -> Hash
hashAccount (addr, amount) = hash $ addr `appendByteString` "#" `appendByteString` (stringToBuiltinByteString $ show amount)
{-# INLINEABLE hashAccount #-}

combineHash :: Hash -> Hash -> Hash
combineHash h h' = hash (appendByteString h h')
{-# INLINEABLE combineHash #-}

newtype Root = Root Hash

withdraw :: Account -> Root -> Proof -> Maybe Root
withdraw account@(addr, _) (Root root) = go (hashAccount account) (hashAccount account')
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
