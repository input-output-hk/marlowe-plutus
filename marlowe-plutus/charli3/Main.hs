{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Create and export a Charli3 oracle.
module Main (
  -- * Entry point.
  main,
) where

import Cardano.Binary (serialize')

import qualified Data.ByteString as BS (writeFile)
import qualified Data.ByteString.Base16 as B16 (encode)
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import qualified Language.Marlowe.Plutus.Charli3 as Charli3 (validatorBytes, validatorHash)
import qualified Options.Applicative as O

-- | The command-line arguments.
data Command = Command
  { charli3CurrencySymbol :: !V1.CurrencySymbol
  , charli3TokenName :: !V1.TokenName
  , oracleChoiceName :: !V1.ChoiceName
  , outFilename :: !FilePath
  }
  deriving (Show)

commandParser :: IO (O.ParserInfo Command)
commandParser =
  do
    let commandOptions =
          Command
            <$> O.strArgument
              ( O.metavar "CURRENCY_SYMBOL"
                  <> O.help
                    "The currency symbol for the Charli3 oracle reference input token. For example, \"30d7c4da385a1f5044261d27b6a22d46b645ca3567636df5edeb303d\" on mainnet and \"e4c846f0f87a7b4524d8e7810ed957c6b7f6e4e2e2e42d75ffe7b373\" on preprod."
              )
            <*> O.strArgument
              ( O.metavar "TOKEN_NAME"
                  <> O.help "The token name for the Charli3 oracle reference input token. For example, \"OracleFeed\"."
              )
            <*> O.strArgument
              ( O.metavar "CHOICE_NAME"
                  <> O.help "The Marlowe choice name for the oracle input to the contract. For example, \"Charli3 ADAUSD\"."
              )
            <*> O.strArgument (O.metavar "FILE_NAME" <> O.help "The name of the file for the Plutus script.")
    pure $
      O.info
        (O.helper {- <*> O.versionOption -} <*> commandOptions)
        ( O.fullDesc
            <> O.progDesc
              "This command-line tool outputs a Plutus script that bridges a Charli3 oracle to a Marlowe contract."
            <> O.header "marlowe-charli3 : run a Charli3 oracle bridge for Marlowe contracts"
        )

-- | Create and export a Charli3 oracle.
main :: IO ()
main =
  do
    Command{..} <- O.execParser =<< commandParser
    print $ Charli3.validatorHash charli3CurrencySymbol charli3TokenName oracleChoiceName
    BS.writeFile outFilename $
      "{\"type\": \"PlutusScriptV2\", \"description\": \"\", \"cborHex\": \""
        <> B16.encode (serialize' $ Charli3.validatorBytes charli3CurrencySymbol charli3TokenName oracleChoiceName)
        <> "\"}"
