{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (
  main,
) where

import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as C
import qualified Cardano.Crypto.Hash as Hash
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import Language.Marlowe.PlutusSpec (ScriptsInfo (..), specForScript)
import qualified Plutus.V1.Ledger.Address as P (scriptHashAddress)
import qualified Plutus.V2.Ledger.Api as P
import System.Environment (getArgs)
import Test.Hspec.Core.Runner (
  defaultConfig,
  evaluateSummary,
  readConfig,
  runSpec,
 )

main :: IO ()
main =
  getArgs
    >>= \case
      semanticsFile : payoutFile : flags ->
        do
          scripts <- mkScriptsInfo semanticsFile payoutFile
          config <- readConfig defaultConfig flags
          summary <- flip runSpec config $ specForScript scripts
          evaluateSummary summary
      _ ->
        putStrLn
          "USAGE: marlowe-plutus <semantics validator text envelope file> <payoutValidator text envelope file> [hspec flags]"

mkScriptsInfo
  :: FilePath
  -> FilePath
  -> IO ScriptsInfo
mkScriptsInfo semanticsFile payoutFile =
  do
    (semanticsValidatorBytes, semanticsValidatorHash, semanticsAddress) <- readScript semanticsFile
    (payoutValidatorBytes, payoutValidatorHash, payoutAddress) <- readScript payoutFile
    pure ScriptsInfo{..}

readScript
  :: FilePath
  -> IO (P.SerializedScript, P.ValidatorHash, P.Address)
readScript file =
  do
    validatorBytes <-
      C.readFileTextEnvelope (C.AsPlutusScript C.AsPlutusScriptV2) file
        >>= \case
          Right (C.PlutusScriptSerialised plutus) -> pure plutus
          Left e -> error $ "Error parsing validator file " <> show file <> ": " <> show e
    let validatorHash = hashScript validatorBytes
        address = P.scriptHashAddress validatorHash
    pure (validatorBytes, validatorHash, address)

hashScript
  :: P.SerializedScript
  -> P.ValidatorHash
hashScript =
  P.ValidatorHash
    . P.toBuiltin
    . (Hash.hashToBytes :: Hash.Hash Hash.Blake2b_224 SBS.ShortByteString -> BS.ByteString)
    . Hash.hashWith (BS.append "\x02" . SBS.fromShort) -- For Plutus V2.