module Main where

import qualified Data.ByteString as BS
import Data.ByteString.Base16 (decodeBase16)
import qualified Data.ByteString.Short as SBS
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word16, Word64)
import Language.Marlowe.Plutus (serialiseCompiledCode)
import Language.Marlowe.Plutus.RoleTokens (policy)
import Language.Marlowe.Plutus.RoleTokens.Types
import Plutus.V2.Ledger.Api
import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  (txOutRef, roleTokens) <- parseArgs args
  BS.putStr $ SBS.fromShort $ serialiseCompiledCode $ policy (mkRoleTokens roleTokens) txOutRef

parseArgs :: [String] -> IO (TxOutRef, [(TokenName, Integer)])
parseArgs (txIdArg : txIxArg : roleTokensArgs) =
  (,) <$> parseTxOutRef txIdArg txIxArg <*> parseRoleTokens roleTokensArgs
parseArgs _ = fail "Too few arguments"

parseTxOutRef :: String -> String -> IO TxOutRef
parseTxOutRef txIdArg txIxArg =
  TxOutRef <$> parseTxId txIdArg <*> (fromIntegral <$> parseTxIx txIxArg)

parseTxId :: String -> IO TxId
parseTxId = either (fail . T.unpack) (pure . TxId . toBuiltin) . decodeBase16 . encodeUtf8 . T.pack

parseTxIx :: String -> IO Word16
parseTxIx = maybe (fail "Invalid tx ix") pure . readMaybe

parseRoleTokens :: [String] -> IO [(TokenName, Integer)]
parseRoleTokens [] = pure []
parseRoleTokens (nameArg : quantityArg : args) = do
  name <- parseName nameArg
  quantity <- fromIntegral <$> parseQuantity quantityArg
  ((name, quantity) :) <$> parseRoleTokens args
parseRoleTokens _ = fail "Too few arguments"

parseName :: String -> IO TokenName
parseName = either (fail . T.unpack) (pure . TokenName . toBuiltin) . decodeBase16 . encodeUtf8 . T.pack

parseQuantity :: String -> IO Word64
parseQuantity = maybe (fail "Invalid tx ix") pure . readMaybe
