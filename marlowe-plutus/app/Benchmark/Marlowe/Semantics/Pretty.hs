{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Benchmark.Marlowe.Semantics.Pretty (
  writeBenchmarks,
  writeBenchmark,
) where

import Benchmark.Marlowe.Types
import Control.Applicative ((<|>))
import Language.Marlowe.Core.V1.Semantics (MarloweData)
import Language.Marlowe.Scripts.Types (MarloweInput, MarloweTxInput (..))
import PlutusLedgerApi.V1.Value (flattenValue)
import PlutusLedgerApi.V2
import System.Directory
import System.FilePath

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Yaml as Y
import qualified PlutusTx.AssocMap as AM

writeBenchmarks
  :: FilePath
  -> [Benchmark]
  -> IO ()
writeBenchmarks folder benchmarks =
  do
    createDirectoryIfMissing True folder
    sequence_
      [ writeBenchmark (folder </> show txId <.> "yaml") benchmark
      | benchmark <- benchmarks
      , let txId = txInfoId . scriptContextTxInfo $ bScriptContext benchmark
      ]

writeBenchmark
  :: FilePath
  -> Benchmark
  -> IO ()
writeBenchmark filename Benchmark{bScriptContext = ScriptContext{scriptContextTxInfo = TxInfo{..}}, ..} =
  let marloweTxInputToJSON (Input content) =
        A.object
          [ "input" A..= content
          ]
      marloweTxInputToJSON (MerkleizedTxInput content hash) =
        A.object
          [ "input" A..= content
          , "continuation" A..= show hash
          ]
      addressToJSON Address{..} =
        A.object
          [ "payment" A..= show addressCredential
          , "staking" A..= show addressStakingCredential
          ]
      valueToJSON v =
        [ A.object
          [ "currencySymbol" A..= show c
          , "tokenName" A..= (tail . init . show) t
          , "quantity" A..= a
          ]
        | (c, t, a) <- flattenValue v
        ]
      txOutputToJSON TxOut{..} =
        A.object
          [ "address" A..= addressToJSON txOutAddress
          , "value" A..= valueToJSON txOutValue
          , "datum" A..= show txOutDatum
          ]
      txInputToJSON TxInInfo{..} =
        A.object
          [ "txOutRef" A..= (show (txOutRefId txInInfoOutRef) <> "#" <> show (txOutRefIdx txInInfoOutRef))
          , "txOut" A..= txOutputToJSON txInInfoResolved
          ]
   in BS8.writeFile filename
        . Y.encode
        . A.object
        $ [ "txId" A..= show txInfoId
          , "inputs" A..= fmap txInputToJSON txInfoInputs
          , "outputs" A..= fmap txOutputToJSON txInfoOutputs
          , "validityInterval"
              A..= case txInfoValidRange of
                Interval (LowerBound (Finite (POSIXTime l)) _) (UpperBound (Finite (POSIXTime h)) _) ->
                  pure $
                    A.object
                      [ "invalidBefore" A..= l
                      , "invalidHereafter" A..= h
                      ]
                _ -> Nothing
          , "redeemer" A..= fmap (fmap marloweTxInputToJSON) (fromData bRedeemer :: Maybe MarloweInput)
          , "datums"
              A..= M.fromList
                [ ( T.pack $ show dh
                  , marloweData <|> payoutData <|> pure (A.toJSON $ show d)
                  )
                | (dh, Datum d) <- AM.toList txInfoData
                , let marloweData = fmap A.toJSON (fromBuiltinData d :: Maybe MarloweData)
                , let payoutData =
                        (\(c, t) -> A.object ["currencySymbol" A..= show c, "tokenName" A..= (tail . init . show) t])
                          <$> (fromBuiltinData d :: Maybe (CurrencySymbol, TokenName))
                ]
          ]
