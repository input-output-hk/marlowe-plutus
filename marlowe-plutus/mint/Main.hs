{-# LANGUAGE RecordWildCards #-}

module Main (
  -- * Entry point
  main,
) where

import Data.Aeson (FromJSON (..), FromJSONKey (..), eitherDecodeStrict, withText)
import Data.Aeson.Types (FromJSONKeyFunction (..), Parser)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 (decodeBase16)
import Data.Map (Map)
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word16, Word64)
import GHC.Generics (Generic)
import Language.Marlowe.Plutus.RoleTokens (policy)
import Language.Marlowe.Plutus.RoleTokens.Types (mkRoleTokens)
import PlutusLedgerApi.V2 (
  TokenName (TokenName),
  TxId (TxId),
  TxOutRef (TxOutRef),
  serialiseCompiledCode,
  toBuiltin,
 )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import qualified Data.Map as Map
import qualified Data.Text as T

newtype Base16 = Base16 {unBase16 :: ByteString}
  deriving (Eq, Show, Ord)

instance FromJSON Base16 where
  parseJSON = withText "Base16" parseBase16

parseBase16 :: T.Text -> Parser Base16
parseBase16 = either (fail . T.unpack) (pure . Base16) . decodeBase16 . encodeUtf8

instance FromJSONKey Base16 where
  fromJSONKey = FromJSONKeyTextParser parseBase16

data Input = Input
  { seedInputId :: Base16
  , seedInputIx :: Word16
  , roleTokens :: Map Base16 Word64
  }
  deriving (Generic)

instance FromJSON Input

main :: IO ()
main = do
  line <- BS.getLine
  Input{..} <- either fail pure $ eitherDecodeStrict line
  let roleTokens' =
        Map.toAscList
          . Map.mapKeysMonotonic (TokenName . toBuiltin . unBase16)
          . fmap fromIntegral
          $ roleTokens
      txOutRef = TxOutRef (TxId $ toBuiltin $ unBase16 seedInputId) (fromIntegral seedInputIx)
  BS.putStr $
    SBS.fromShort $
      serialiseCompiledCode $
        policy (mkRoleTokens roleTokens') txOutRef
