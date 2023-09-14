-- | Types for benchmarking Marlowe validators.
--
-- Module      :  Benchmark.Marlowe.Types
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
module Benchmark.Marlowe.Types (
  -- * Benchmarking
  Benchmark (..),
  makeBenchmark,
) where

import Plutus.V2.Ledger.Api (Data, ExBudget, ScriptContext, ToData, toData)

-- | A benchmarking case.
data Benchmark = Benchmark
  { bDatum :: Data
  -- ^ The datum.
  , bRedeemer :: Data
  -- ^ The redeemer.
  , bScriptContext :: ScriptContext
  -- ^ The script context.
  , bReferenceCost :: Maybe ExBudget
  -- ^ The previously measured execution costs.
  }
  deriving (Show)

-- | Construct a benchmarking case.
makeBenchmark
  :: (ToData d)
  => (ToData r)
  => d
  -> r
  -> ScriptContext
  -> Maybe ExBudget
  -> Benchmark
makeBenchmark datum redeemer = Benchmark (toData datum) (toData redeemer)
