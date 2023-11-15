{-# LANGUAGE OverloadedStrings #-}

-- | Run benchmarks for Marlowe validators.
--
-- Module      :  Main
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
module Main (
  -- * Entry point
  main,
) where

import Benchmark.Marlowe (tabulateResults)
import qualified Benchmark.Marlowe.RolePayout as RolePayout (benchmarks, validatorBytes, validatorHash)
import qualified Benchmark.Marlowe.Semantics as Semantics (benchmarks, validatorBytes, validatorHash)
import Cardano.Binary (serialize')
import qualified Data.ByteString as BS (writeFile)
import qualified Data.ByteString.Base16 as B16 (encode)
import Data.List (intercalate)
import Language.Marlowe.Plutus (openRoleValidatorBytes, openRoleValidatorHash)
import PlutusLedgerApi.V2 (ScriptHash, SerialisedScript)

-- | Run the benchmarks and export information about the validators and the benchmarking results.
main :: IO ()
main =
  do
    -- Read the semantics benchmarks.
    benchmarks <- either error id <$> Semantics.benchmarks

    -- Write the tabulation of semantics benchmark results.
    writeFile "marlowe-semantics.tsv"
      . unlines
      . fmap (intercalate "\t")
      $ tabulateResults "Semantics" Semantics.validatorHash Semantics.validatorBytes benchmarks

    {-
        -- Write the flat UPLC files for the semantics benchmarks.
        writeFlatUPLCs Semantics.writeUPLC benchmarks
          . (</> "semantics")
          =<< getDataDir
    -}

    -- Print the semantics validator, and write the plutus file.
    printValidator
      "Semantics"
      "marlowe-semantics"
      Semantics.validatorHash
      Semantics.validatorBytes

    -- Read the role-payout benchmarks.
    benchmarks' <- either error id <$> RolePayout.benchmarks

    -- Write the tabulation of role-payout benchmark results.
    writeFile "marlowe-rolepayout.tsv"
      . unlines
      . fmap (intercalate "\t")
      $ tabulateResults "Role Payout" RolePayout.validatorHash RolePayout.validatorBytes benchmarks'

    {-
        -- Write the flat UPLC files for the role-payout benchmarks.
        writeFlatUPLCs RolePayout.writeUPLC benchmarks'
          . (</> "rolepayout")
          =<< getDataDir
    -}

    -- Print the role-payout validator, and write the plutus file.
    printValidator
      "Role payout"
      "marlowe-rolepayout"
      RolePayout.validatorHash
      RolePayout.validatorBytes

    -- Print the open-role validator, and write the plutus file.
    printValidator
      "Open role"
      "open-role"
      openRoleValidatorHash
      openRoleValidatorBytes

-- | Print information about a validator.
printValidator
  :: String
  -- ^ The name of the validator.
  -> FilePath
  -- ^ The base file path for exported files.
  -> ScriptHash
  -- ^ The hash of the validator script.
  -> SerialisedScript
  -- ^ The serialised validator.
  -> IO ()
  -- ^ Action to print the information about the benchmarking, and write the files.
printValidator name file hash validator =
  do
    putStrLn $ name <> ":"
    putStrLn $ "  Validator hash: " <> show hash
    putStrLn $ "  Validator file: " <> file <> ".plutus"
    putStrLn $ "  Measurements file: " <> file <> ".tsv"
    BS.writeFile (file <> ".plutus") $
      "{\"type\": \"PlutusScriptV2\", \"description\": \"\", \"cborHex\": \""
        <> B16.encode (serialize' validator)
        <> "\"}"
    putStrLn ""
