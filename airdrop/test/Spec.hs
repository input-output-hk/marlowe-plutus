{-# LANGUAGE BlockArguments #-}

module Main where

import qualified Spec.Airdrop.TokenAllocationTree
import qualified Spec.Airdrop.Validator
import Test.Hspec (hspec)

main :: IO ()
main = do
  hspec do
    Spec.Airdrop.TokenAllocationTree.spec
    Spec.Airdrop.Validator.spec
