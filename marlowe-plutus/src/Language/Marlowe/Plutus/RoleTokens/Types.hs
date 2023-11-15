{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Language.Marlowe.Plutus.RoleTokens.Types (
  MintAction (..),
  RoleTokens,
  RoleTokensHash,
  mkRoleTokens,
  mkRoleTokensHash,
) where

import GHC.Generics (Generic)
import qualified PlutusLedgerApi.V2 as PV2
import PlutusTx (makeIsDataIndexed)
import PlutusTx.Builtins (serialiseData)
import PlutusTx.Lift (makeLift)
import PlutusTx.Prelude hiding (last)
import qualified Prelude as Haskell

-- | Invariant: internal list has unique token name entries and is sorted.
newtype RoleTokens = RoleTokens [(PV2.TokenName, Integer)]
  deriving stock (Haskell.Show, Haskell.Eq, Generic)

makeLift ''RoleTokens
makeIsDataIndexed ''RoleTokens [('RoleTokens, 0)]

instance Eq RoleTokens where
  {-# INLINEABLE (==) #-}
  (RoleTokens r1) == (RoleTokens r2) = r1 == r2

{-# INLINEABLE mkRoleTokens #-}
mkRoleTokens :: [(PV2.TokenName, Integer)] -> RoleTokens
mkRoleTokens = RoleTokens . squash . sort
  where
    squash = finalize . foldr step Nothing

    finalize Nothing = []
    finalize (Just (tokAcc, acc)) = tokAcc : acc

    -- Unfortunatelly `PlutusTx.Prelude` doesn't provide `group` so we perform
    -- this fold manually here.
    step last Nothing = Just (last, [])
    step curr@(tn', i') (Just (tokAcc@(tn, i), acc)) =
      Just
        $ if tn == tn'
          then ((tn, i + i'), acc)
          else (curr, tokAcc : acc)

newtype RoleTokensHash = RoleTokensHash {getRoleTokensHash :: PV2.BuiltinByteString}
  deriving (Haskell.Show) via PV2.LedgerBytes
  deriving stock (Generic)
  deriving newtype (Haskell.Eq, Haskell.Ord, Eq, Ord, PV2.ToData, PV2.FromData, PV2.UnsafeFromData)

makeLift ''RoleTokensHash

mkRoleTokensHash :: RoleTokens -> RoleTokensHash
mkRoleTokensHash = RoleTokensHash . sha2_256 . serialiseData . PV2.toBuiltinData

data MintAction = Mint | Burn
  deriving stock (Haskell.Show, Haskell.Eq, Generic)

instance Eq MintAction where
  {-# INLINEABLE (==) #-}
  Burn == Burn = True
  Mint == Mint = True
  _ == _ = False

makeIsDataIndexed ''MintAction [('Mint, 0), ('Burn, 1)]
makeLift ''MintAction
