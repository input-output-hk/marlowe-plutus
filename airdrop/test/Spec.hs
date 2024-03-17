module Main where

import qualified Spec.Airdrop.TokenAllocationTree
import Test.Hspec (hspec)

main :: IO ()
main = do
  tokenAllocationTreeSpec <- Spec.Airdrop.TokenAllocationTree.mkSpec
  hspec tokenAllocationTreeSpec
