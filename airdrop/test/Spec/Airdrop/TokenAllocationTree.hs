{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Airdrop.TokenAllocationTree where

import Airdrop.TokenAllocationTree (
  AllocationIx (AllocationIx),
  Height (Height),
  Recipient (Recipient),
  Step (..),
  TokenAllocation (TokenAllocation),
  TokenAmount (TokenAmount),
  TopDown (TopDown),
  WithdrawError (AllocationAlreadyWithdrawn, AllocationNotFound),
  createTables,
  inefficientRemainingAllocations,
  mkAllocationProof,
  mkNodeTopDownPath,
  mkRecipient,
  mkTokenAmount,
  populate,
  unsafeMkRecipient,
  unsafeMkTokenAmount,
  unusableRecipient,
  unusableTokenAllocation,
  withdraw,
 )
import Control.Monad (foldM, foldM_, void)
import Control.Monad.Error.Class (liftEither)
import Data.Bifunctor (first)
import qualified Data.ByteString.Char8 as Char8
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.List (nub, partition, sort)
import qualified Data.List.NonEmpty as NonEmptyList
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Traversable (for)
import Database.SQLite.Simple (open)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (arbitrary), NonNegative (getNonNegative), Testable (property), suchThat, suchThatMap)
import Test.QuickCheck.Gen (shuffle)
import Test.QuickCheck.Instances.ByteString ()

mkTokenAllocations :: Int -> NonEmptyList.NonEmpty TokenAllocation
mkTokenAllocations numberOfAllocations = do
  let mkTokenAllocation i = TokenAllocation recipient tokenAmount
        where
          recipient = unsafeMkRecipient $ "b" <> Char8.pack (show i)
          tokenAmount = unsafeMkTokenAmount $ fromIntegral i * 100
      head = mkTokenAllocation 0
  if numberOfAllocations == 1
    then head NonEmptyList.:| []
    else head NonEmptyList.:| ([1 .. (numberOfAllocations - 1)] <&> mkTokenAllocation)

newtype ArbitraryTokenAllocation = ArbitraryTokenAllocation {unArbitraryTokenAllocation :: TokenAllocation}
  deriving newtype (Ord, Eq, Show)

newtype ArbitraryRecipient = ArbitraryRecipient {unArbitraryRecipient :: Recipient}
  deriving newtype (Ord, Eq, Show)

instance Arbitrary ArbitraryRecipient where
  arbitrary = do
    recipient <- arbitrary `suchThatMap` mkRecipient
    pure $ ArbitraryRecipient recipient

instance Arbitrary ArbitraryTokenAllocation where
  arbitrary = do
    ArbitraryRecipient recipient <- arbitrary
    tokenAmount <- arbitrary `suchThatMap` mkTokenAmount
    pure $ ArbitraryTokenAllocation $ TokenAllocation recipient tokenAmount

data ShuffledTokenAllocations = ShuffledTokenAllocations (NonEmptyList.NonEmpty TokenAllocation) [Recipient]
  deriving (Show)

instance Arbitrary ShuffledTokenAllocations where
  arbitrary = do
    tokenAllocations <- do
      let go = do
            ts <- arbitrary `suchThat` (not . null)
            ts' <- shuffle . nub . sort $ ts
            case ts' of
              [] -> go
              (h : rest) -> do
                pure $ unArbitraryTokenAllocation <$> h NonEmptyList.:| rest
      go
    recipients <- arbitrary <&> map unArbitraryRecipient
    let getRecipient (TokenAllocation recipient _) = recipient
    withdrawal <- shuffle (fmap getRecipient (NonEmptyList.toList tokenAllocations) <> recipients)
    pure $ ShuffledTokenAllocations tokenAllocations withdrawal

spec :: Spec
spec = do
  let withdraw' conn recipient root = do
        res <- withdraw conn recipient root
        either (error . show) pure res
  describe "TokenAllocationTree" do
    it "should be properly populated for a list of five TokenAllocations" do
      conn <- open ":memory:"
      createTables conn
      -- Let's generate random 5 bytestrings
      let numberOfAllocations = 5
          tokenAllocations = mkTokenAllocations numberOfAllocations
      root <- populate conn tokenAllocations
      leaves <- inefficientRemainingAllocations conn root
      length leaves `shouldBe` 5
      let leaves' = Set.fromList leaves
      for_ tokenAllocations \ta -> Set.member ta leaves' `shouldBe` True

    it "path should be properly computed" do
      mkNodeTopDownPath (Height 3) (AllocationIx 4) `shouldBe` TopDown [GoRight, GoLeft, GoLeft]
      mkNodeTopDownPath (Height 3) (AllocationIx 3) `shouldBe` TopDown [GoLeft, GoRight, GoRight]
      mkNodeTopDownPath (Height 3) (AllocationIx 0) `shouldBe` TopDown [GoLeft, GoLeft, GoLeft]

    it "path should be properly computed for a singleton tree" do
      mkNodeTopDownPath (Height 1) (AllocationIx 0) `shouldBe` TopDown [GoLeft]

    it "should properly withdraw from singleton tree" do
      conn <- open ":memory:"
      createTables conn
      let numberOfAllocations = 1
          tokenAllocations = mkTokenAllocations numberOfAllocations
      root <- populate conn tokenAllocations
      case NonEmptyList.toList tokenAllocations of
        [TokenAllocation recipient _] -> do
          remainingAccounts1 <- inefficientRemainingAllocations conn root
          length remainingAccounts1 `shouldBe` 1

          newRoot <- withdraw' conn recipient root
          remainingAccounts2 <- inefficientRemainingAllocations conn newRoot
          length remainingAccounts2 `shouldBe` 0
        _ -> fail "No token allocations"

    it "should properly withdraw from an account" do
      conn <- open ":memory:"
      createTables conn
      let numberOfAllocations = 5
          tokenAllocations = mkTokenAllocations numberOfAllocations
      root <- populate conn tokenAllocations

      case NonEmptyList.toList tokenAllocations of
        [ TokenAllocation recipient1 _
          , TokenAllocation recipient2 _
          , TokenAllocation recipient3 _
          , TokenAllocation recipient4 _
          , TokenAllocation recipient5 _
          ] -> do
            newRoot1 <- withdraw' conn recipient1 root
            remainingAccounts1 <- inefficientRemainingAllocations conn newRoot1
            length remainingAccounts1 `shouldBe` 4

            newRoot2 <- withdraw' conn recipient3 newRoot1
            remainingAccounts2 <- inefficientRemainingAllocations conn newRoot2
            length remainingAccounts2 `shouldBe` 3

            newRoot3 <- withdraw' conn recipient5 newRoot2
            remainingAccounts3 <- inefficientRemainingAllocations conn newRoot3
            length remainingAccounts3 `shouldBe` 2

            newRoot4 <- withdraw' conn recipient4 newRoot3
            remainingAccounts4 <- inefficientRemainingAllocations conn newRoot4
            length remainingAccounts4 `shouldBe` 1

            newRoot5 <- withdraw' conn recipient2 newRoot4
            remainingAccounts5 <- inefficientRemainingAllocations conn newRoot5
            length remainingAccounts5 `shouldBe` 0

            newRoot6 <- withdraw conn recipient2 newRoot5
            newRoot6 `shouldBe` Left AllocationAlreadyWithdrawn

            newRoot7 <- withdraw conn (unsafeMkRecipient "non-existing") newRoot5
            newRoot7 `shouldBe` Left AllocationNotFound
        _ -> fail "No token allocations"

    -- Let's now test withdrawals over random trees
    prop "should properly process random withdrawal sets" \(ShuffledTokenAllocations tokenAllocations withdrawals) -> do
      let tokenAllocationRecipient (TokenAllocation recipient _) = recipient
          recipients = NonEmptyList.toList $ fmap tokenAllocationRecipient tokenAllocations

      conn <- open ":memory:"
      createTables conn
      root <- populate conn tokenAllocations
      let withdraw' (root, remainingAllocations) recipient = do
            res <- withdraw conn recipient root
            case (recipient `elem` remainingAllocations, recipient `elem` recipients, res) of
              (True, _, Right root') -> pure (root', filter (/= recipient) remainingAllocations)
              (False, False, Left AllocationNotFound) -> pure (root, remainingAllocations)
              (False, True, Left AllocationAlreadyWithdrawn) -> pure (root, remainingAllocations)
              t -> do
                leaves <- inefficientRemainingAllocations conn root
                fail $ "Unexpected result: " <> show (t, recipient, leaves)

      foldM_ withdraw' (root, recipients) withdrawals
