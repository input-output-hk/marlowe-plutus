{-# LANGUAGE OverloadedStrings #-}

module Main where

import Airdrop.TokenAllocationTree (createTables)
import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

data TestField = TestField Int String deriving (Show)

instance FromRow TestField where
  fromRow = TestField <$> field <*> field

main :: IO ()
main = do
  conn <- open "airdrop.db"
  createTables conn
  close conn
