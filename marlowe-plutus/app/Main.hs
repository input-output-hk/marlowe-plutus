{-# LANGUAGE OverloadedStrings #-}

-- | Run benchmarks for Marlowe validators.
module Main (
  -- * Entry point
  main,
) where

import Benchmark.Marlowe (tabulateResults, writeFlatUPLCs)
import Cardano.Binary (serialize')
import Data.List (intercalate)
import Language.Marlowe.Plutus.OpenRoles (openRoleValidatorBytes, openRoleValidatorHash)
import Paths_marlowe_plutus (getDataDir)
import PlutusLedgerApi.V2 (ScriptHash, SerialisedScript)
import System.FilePath ((</>))

import qualified Benchmark.Marlowe.RolePayout as RolePayout (
  benchmarks,
  validatorBytes,
  validatorHash,
  writeUPLC,
 )
import qualified Benchmark.Marlowe.Semantics as Semantics (
  benchmarks,
  validatorBytes,
  validatorHash,
  writeUPLC,
 )
import qualified Data.ByteString as BS (writeFile)
import qualified Data.ByteString.Base16 as B16 (encode)

-- | Run the benchmarks and export information about the validators and the benchmarking results.
main :: IO ()
main =
  do
    dir <- getDataDir

    -- Read the semantics benchmarks.
    benchmarks <- either error id <$> Semantics.benchmarks

    -- Write the tabulation of semantics benchmark results.
    writeFile "marlowe-semantics.tsv"
      . unlines
      . fmap (intercalate "\t")
      $ tabulateResults "Semantics" Semantics.validatorHash Semantics.validatorBytes benchmarks

    -- Write the flat UPLC files for the semantics benchmarks.
    writeFlatUPLCs Semantics.writeUPLC benchmarks $
      dir </> "semantics"

    -- Print the semantics validator, and write the plutus file.
    printValidator
      "Semantics"
      (dir <> "marlowe-semantics")
      Semantics.validatorHash
      Semantics.validatorBytes

    -- Read the role-payout benchmarks.
    benchmarks' <- either error id <$> RolePayout.benchmarks

    -- Write the tabulation of role-payout benchmark results.
    writeFile "marlowe-rolepayout.tsv"
      . unlines
      . fmap (intercalate "\t")
      $ tabulateResults "Role Payout" RolePayout.validatorHash RolePayout.validatorBytes benchmarks'

    -- Write the flat UPLC files for the role-payout benchmarks.
    writeFlatUPLCs RolePayout.writeUPLC benchmarks' $
      dir </> "rolepayout"

    -- Print the role-payout validator, and write the plutus file.
    printValidator
      "Role payout"
      "marlowe-rolepayout"
      RolePayout.validatorHash
      RolePayout.validatorBytes

    -- Print the semantics validator, and write the plutus file.
    printValidator
      "Open roles"
      (dir <> "marlowe-openroles")
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
