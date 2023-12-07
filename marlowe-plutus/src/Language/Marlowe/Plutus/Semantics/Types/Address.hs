module Language.Marlowe.Plutus.Semantics.Types.Address (
  -- * Types
  Network,
  mainnet,
  testnet,
) where

-- | Type of network.
type Network = Bool

-- | The main network.
mainnet :: Network
mainnet = toEnum 0x01

-- | A test network.
testnet :: Network
testnet = toEnum 0x00
