{-# LANGUAGE OverloadedStrings #-}

module Main where

import Cardano.Binary (serialize')

import qualified Data.ByteString as BS (writeFile)
import qualified Data.ByteString.Base16 as B16 (encode)
import qualified Language.Marlowe.Scripts.Charli3 as Charli3 (validatorBytes, validatorHash)

main :: IO ()
main =
  do
    putStrLn $ "  Validator hash: " <> show Charli3.validatorHash
    putStrLn "  Validator file: charli3.plutus"
    BS.writeFile "charli3.plutus" $
      "{\"type\": \"PlutusScriptV2\", \"description\": \"\", \"cborHex\": \""
        <> B16.encode (serialize' Charli3.validatorBytes)
        <> "\"}"
