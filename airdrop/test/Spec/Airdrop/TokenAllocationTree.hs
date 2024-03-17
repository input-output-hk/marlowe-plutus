{-# LANGUAGE BlockArguments #-}
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
  TotalNumberOfLeaves (TotalNumberOfLeaves),
  createTables,
  inefficientLeavesByRoot,
  mkAllocationProof,
  mkNodeTopDownPath,
  populate,
  withdraw,
 )
import qualified Data.ByteString.Char8 as Char8
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.List (partition)
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set
import Data.Traversable (for)
import Database.SQLite.Simple (open)
import Test.Hspec (Spec, describe, it, shouldBe)

mkTokenAllocations :: Int -> [TokenAllocation]
mkTokenAllocations numberOfAllocations = do
  [1 .. numberOfAllocations] <&> \i -> do
    let recipient = Recipient $ "b" <> Char8.pack (show i)
        tokenAmount = TokenAmount $ fromIntegral i * 100
    TokenAllocation recipient tokenAmount

mkSpec :: IO Spec
mkSpec = do
  pure do
    describe "TokenAllocationTree" do
      it "should be properly populated for a list of five TokenAllocations" do
        conn <- open ":memory:"
        createTables conn
        -- Let's generate random 5 bytestrings
        let numberOfAllocations = 5
            tokenAllocations = mkTokenAllocations numberOfAllocations
        root <- populate conn tokenAllocations
        leaves <- inefficientLeavesByRoot conn root
        length leaves `shouldBe` 8
        let leaves' = Set.fromList $ leaves <&> \(r, a, _) -> (r, a)

        for_ tokenAllocations \ta -> do
          case ta of
            TokenAllocation recipient amount -> do
              Set.member (recipient, amount) leaves' `shouldBe` True

      it "path should be properly computed" do
        let totalNumberOfLeaves = TotalNumberOfLeaves 8

        mkNodeTopDownPath (Height 3) (AllocationIx 4) `shouldBe` TopDown [GoRight, GoLeft, GoLeft]
        mkNodeTopDownPath (Height 3) (AllocationIx 3) `shouldBe` TopDown [GoLeft, GoRight, GoRight]
        mkNodeTopDownPath (Height 3) (AllocationIx 0) `shouldBe` TopDown [GoLeft, GoLeft, GoLeft]

      it "should properly withdraw from an account" do
        conn <- open ":memory:"
        createTables conn
        let numberOfAllocations = 5
            tokenAllocations = mkTokenAllocations numberOfAllocations
        root <- populate conn tokenAllocations

        case tokenAllocations of
          [ TokenAllocation recipient1 _
            , TokenAllocation recipient2 _
            , TokenAllocation recipient3 _
            , TokenAllocation recipient4 _
            , TokenAllocation recipient5 _
            ] -> do
              newRoot1 <- withdraw conn recipient1 root
              (nonEmptyAccounts1, emptyAccounts1) <-
                inefficientLeavesByRoot conn newRoot1 <&> partition \(_, amount, _) ->
                  amount == (mempty :: TokenAmount)
              length emptyAccounts1 `shouldBe` 4
              length nonEmptyAccounts1 `shouldBe` 4

              newRoot2 <- withdraw conn recipient2 newRoot1
              (nonEmptyAccounts2, emptyAccounts2) <-
                inefficientLeavesByRoot conn newRoot2 <&> partition \(_, amount, _) ->
                  amount == (mempty :: TokenAmount)
              length emptyAccounts2 `shouldBe` 3
              length nonEmptyAccounts2 `shouldBe` 5

              newRoot3 <- withdraw conn recipient3 newRoot2
              (nonEmptyAccounts3, emptyAccounts3) <-
                inefficientLeavesByRoot conn newRoot3 <&> partition \(_, amount, _) ->
                  amount == (mempty :: TokenAmount)
              length emptyAccounts3 `shouldBe` 2
              length nonEmptyAccounts3 `shouldBe` 6

              newRoot4 <- withdraw conn recipient4 newRoot3
              (nonEmptyAccounts4, emptyAccounts4) <-
                inefficientLeavesByRoot conn newRoot4 <&> partition \(_, amount, _) ->
                  amount == (mempty :: TokenAmount)

              length emptyAccounts4 `shouldBe` 1
              length nonEmptyAccounts4 `shouldBe` 7

              newRoot5 <- withdraw conn recipient5 newRoot4
              (nonEmptyAccounts5, emptyAccounts5) <-
                inefficientLeavesByRoot conn newRoot5 <&> partition \(_, amount, _) ->
                  amount == (mempty :: TokenAmount)

              length emptyAccounts5 `shouldBe` 0
              length nonEmptyAccounts5 `shouldBe` 8
          _ -> fail "No token allocations"
