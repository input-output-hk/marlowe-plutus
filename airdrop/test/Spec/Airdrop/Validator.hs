{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module Spec.Airdrop.Validator where

import Airdrop.TokenAllocationTree (
  AllocationIx (AllocationIx),
  Height (Height),
  Recipient (Recipient),
  Step (..),
  TokenAllocation (TokenAllocation),
  TokenAmount (TokenAmount),
  TopDown (TopDown),
  createTables,
  mkAllocationProof,
  mkNodeTopDownPath,
  populate,
  withdraw,
 )

import qualified Airdrop.Validator as Validator
import Control.Monad (replicateM)
import Control.Monad.Trans.Except (runExcept)
import Control.Monad.Trans.Writer (WriterT (runWriterT))
import Data.Bifunctor (bimap)
import qualified Data.ByteString.Char8 as Char8
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.List (partition)
import Data.Maybe (listToMaybe)
import qualified Data.Maybe as PlutusTx.Maybe
import qualified Data.Set as Set
import Data.Traversable (for)
import Database.SQLite.Simple (open)
import PlutusLedgerApi.Common (LogOutput, serialiseCompiledCode)
import PlutusLedgerApi.Common.Versions (futurePV)
import PlutusLedgerApi.V2 (
  Address,
  BuiltinData,
  Data (..),
  EvaluationContext,
  ExBudget (..),
  ScriptHash,
  SerialisedScript,
  ToData (toBuiltinData),
  UnsafeFromData (unsafeFromBuiltinData),
  VerboseMode (Verbose),
  deserialiseScript,
  evaluateScriptCounting,
  mkEvaluationContext,
  toData,
 )
import qualified PlutusLedgerApi.V2 as PlutusLedgerApi
import qualified PlutusTx
import PlutusTx.Prelude (check, toBuiltin)
import qualified PlutusTx.Prelude as P
import PlutusTx.These (These (..))
import Spec.Airdrop.Validator.CostModel (costModel)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Arbitrary, arbitrary, generate)
import Test.QuickCheck.Gen (Gen, suchThat)
import Test.QuickCheck.Instances.ByteString ()

evaluationContext :: Either String EvaluationContext
evaluationContext = bimap show fst . runExcept . runWriterT . mkEvaluationContext $ snd <$> costModel

-- | Dump benchmarking files.
dumpBenchmarks :: Bool
dumpBenchmarks = False

-- | Check the Plutus execution budget.
enforceBudget :: Bool
enforceBudget = False

withdrawScript :: SerialisedScript
withdrawScript = serialiseCompiledCode compiledWithdraw
  where
    -- Compiled version of withdrawal which throws an error if proof is invalid.
    compiledWithdraw :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> ())
    compiledWithdraw = do
      let withdraw' :: BuiltinData -> BuiltinData -> ()
          withdraw' withdrawal root =
            check $
              PlutusTx.Maybe.isJust $
                Validator.withdraw
                  (unsafeFromBuiltinData withdrawal)
                  (unsafeFromBuiltinData root)
      $$(PlutusTx.compile [||withdraw'||])

evaluatePlutusScript
  :: SerialisedScript
  -- ^ The scripts being tested.
  -> [Data]
  -- ^ Arguments.
  -> These String LogOutput
  -- ^ The result.
evaluatePlutusScript serialisedScript args =
  case deserialiseScript futurePV serialisedScript of
    Left message -> This $ show message
    Right script ->
      case evaluationContext of
        Left message -> This message
        Right ec -> case evaluateScriptCounting futurePV Verbose ec script args of
          (logOutput, Right ex@ExBudget{..}) ->
            if enforceBudget && (exBudgetCPU > 10_000_000_000 || exBudgetMemory > 14_000_000)
              then These ("Exceeded Plutus budget: " <> show ex) logOutput
              else That logOutput
          (logOutput, Left message) -> These (show message) logOutput

newtype ValidatorBytes = ValidatorBytes {validatorBytes :: SerialisedScript}
  deriving (Show)

evaluateValidator
  :: ValidatorBytes
  -- ^ The scripts being tested.
  -> Data
  -- ^ The datum.
  -> Data
  -- ^ The redeemer.
  -> Data
  -- ^ The script context.
  -> These String LogOutput
  -- ^ The result.
evaluateValidator ValidatorBytes{validatorBytes} d r c =
  evaluatePlutusScript validatorBytes [d, r, c]

newtype ProofLength = ProofLength {unProofLength :: Int}
  deriving newtype (Eq, Ord, Show)

newtype ArbitraryHash = ArbitraryHash {unArbitraryHash :: Validator.Hash}
  deriving newtype (Eq, Ord, Show)

instance Arbitrary ArbitraryHash where
  arbitrary = do
    (preimage :: Char8.ByteString) <- arbitrary `suchThat` (/= mempty)
    pure $ ArbitraryHash $ Validator.hash $ PlutusTx.Prelude.toBuiltin preimage

arbitraryWithdrawalArgs :: ProofLength -> Gen (Validator.Withdrawal, Validator.Root)
arbitraryWithdrawalArgs (ProofLength proofLength) = do
  pubKeyHash <- PlutusLedgerApi.PubKeyHash . unArbitraryHash <$> arbitrary
  amount <- arbitrary `suchThat` (> 0)
  let tokenAllocation = (pubKeyHash, amount)
  siblingsBottomUp <- replicateM proofLength do
    h <- unArbitraryHash <$> arbitrary
    arbitrary >>= \case
      True -> pure $ Left h
      False -> pure $ P.Right h
  let deriveRoot root [] = root
      deriveRoot subRoot (sibling : siblings) = do
        let subRoot' = case sibling of
              Left h -> Validator.combineHash h subRoot
              Right h -> Validator.combineHash subRoot h
        deriveRoot subRoot' siblings
      origRoot = deriveRoot (Validator.hashTokenAllocation tokenAllocation) siblingsBottomUp
  -- newRoot = deriveRoot unusableTokenAllocationHash siblingsBottomUp
  pure ((tokenAllocation, siblingsBottomUp), Validator.Root origRoot)

spec :: Spec
spec = do
  describe "Airdrop.Validator" do
    it "using minimal script context evaluates withdraw correctly" do
      (withdrawal, root) <- generate (arbitraryWithdrawalArgs (ProofLength 2))
      print withdrawal
      print root

      print "RESULT:"
      print $ Validator.withdraw withdrawal root
      case evaluatePlutusScript withdrawScript [toData withdrawal, toData root] of
        This message -> print message
        That logOutput -> print logOutput
        These message logOutput -> do
          print message
          print logOutput
