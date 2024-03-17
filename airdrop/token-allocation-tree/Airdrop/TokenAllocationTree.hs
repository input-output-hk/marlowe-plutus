{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Marlowe semantics validator.
module Airdrop.TokenAllocationTree (
  createTables,
  fromValidatorTokenAllocation,
  hashTokenAllocation,
  inefficientRemainingAllocations,
  mkTokenAmount,
  mkAllocationProof,
  mkNodeTopDownPath,
  mkRecipient,
  populate,
  toValidatorTokenAllocation,
  unusableRecipient,
  unusableTokenAllocation,
  unsafeMkRecipient,
  unsafeMkTokenAmount,
  withdraw,
  AllocationIx (..),
  AllocationProof (..),
  Hash (..),
  Height (..),
  Recipient (Recipient),
  Root (..),
  Step (..),
  TokenAllocation (..),
  TokenAmount (TokenAmount),
  TopDown (..),
  WithdrawError (..),
)
where

import qualified Airdrop.Validator as Validator
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.List (sortOn)
import qualified Data.List.NonEmpty as NonEmptyList
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Traversable (for)
import Database.SQLite.Simple (Connection, FromRow, Only (..), ToRow, execute, execute_, query, query_, withTransaction)
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.FromRow (FromRow (fromRow), field)
import Database.SQLite.Simple.ToField (ToField)
import Database.SQLite.Simple.ToRow (ToRow (toRow))
import GHC.Generics (Generic)
import qualified PlutusLedgerApi.V2 as Plutus
import Prelude

newtype Hash = Hash ByteString
  deriving newtype (Eq, Ord, Show, FromField, ToField)

newtype Height = Height Integer
  deriving (Eq, Ord, Show)

data Root = Root Hash Height
  deriving (Eq, Ord, Show)

newtype Recipient = Recipient_ ByteString
  deriving newtype (Eq, Ord, Show, FromField, ToField)

{-# COMPLETE Recipient #-}
pattern Recipient :: ByteString -> Recipient
pattern Recipient a <- Recipient_ a

mkRecipient :: ByteString -> Maybe Recipient
mkRecipient bs = if Recipient_ bs == unusableRecipient then Nothing else Just $ Recipient_ bs

-- | If you use empty recipient you would break the invariant.
unsafeMkRecipient :: ByteString -> Recipient
unsafeMkRecipient = Recipient_

newtype TokenAmount = TokenAmount_ Integer
  deriving newtype (Eq, Ord, Show, FromField, ToField)

{-# COMPLETE TokenAmount #-}
pattern TokenAmount :: Integer -> TokenAmount
pattern TokenAmount a <- TokenAmount_ a

mkTokenAmount :: Integer -> Maybe TokenAmount
mkTokenAmount i = if i > 0 then Just $ TokenAmount_ i else Nothing

unsafeMkTokenAmount :: Integer -> TokenAmount
unsafeMkTokenAmount = TokenAmount_

instance Semigroup TokenAmount where
  TokenAmount a <> TokenAmount b = TokenAmount_ (a + b)

instance Monoid TokenAmount where
  mempty = TokenAmount_ 0

data TokenAllocation = TokenAllocation Recipient TokenAmount
  deriving (Eq, Ord, Generic, Show)

fromValidatorTokenAllocation :: Validator.TokenAllocation -> TokenAllocation
fromValidatorTokenAllocation (Plutus.PubKeyHash pkh, amount) =
  TokenAllocation (Recipient_ $ Plutus.fromBuiltin pkh) (TokenAmount_ $ Plutus.fromBuiltin amount)

toValidatorTokenAllocation :: TokenAllocation -> Validator.TokenAllocation
toValidatorTokenAllocation (TokenAllocation (Recipient pkh) (TokenAmount amount)) =
  (Plutus.PubKeyHash $ Plutus.toBuiltin pkh, Plutus.toBuiltin amount)

unusableTokenAllocation :: TokenAllocation
unusableTokenAllocation = fromValidatorTokenAllocation Validator.unusableTokenAllocation

unusableTokenAllocationHash :: Hash
unusableTokenAllocationHash = hashTokenAllocation unusableTokenAllocation

unusableRecipient :: Recipient
unusableRecipient = do
  let TokenAllocation recipient _ = unusableTokenAllocation
  recipient

-- An absolute index of the allocation in the airdrop shard.
newtype AllocationIx = AllocationIx {unAllocationIx :: Integer}
  deriving newtype (Eq, Ord, Show, FromField, ToField)

-- | We want to store merkle tree as an immutable data structure with sharing:
-- |  * Nodes point to their children so we can have new version of the tree which shares the same nodes.
-- |  * Path can be easily computed from the absolute child index and the total number of leaves.
-- |  * The garbage collection or full rebuild (based on the withdrawal history from the chain) can be done pretty easily.
--
-- | We need only two tables:
-- | 1. A table for merkle tree internal nodes: `Node (Hash, Left Hash, Right Hash)`.
-- | 2. A table for merkle tree leaves: `Leaf (Recipient, Hash, TokenAmount, Ix)`.
--
-- | Database consistency:
-- | * We assume that we don't perform any deletions.
-- | * To simplify the schema we don't introduce `FOREIGN KEY` constraints in internal nodes which would be nullable.
-- | * We can consider a trigger which performs a check against both tables when a new row is inserted..
createTables :: Connection -> IO ()
createTables conn = do
  -- We use blobs as they are significantly smalle and there is no difference in performance:
  -- https://sqlite.org/forum/info/69c8cbbe2d84410f
  execute_ conn "CREATE TABLE IF NOT EXISTS Node (hash BLOB PRIMARY KEY, left BLOB, right BLOB)"
  execute_ conn "CREATE TABLE IF NOT EXISTS Leaf (recipient BLOB PRIMARY KEY, hash BLOB, tokenAmount INTEGER, ix INTEGER)"

data InternalNodeRow = InternalNodeRow Hash Hash Hash
  deriving (Eq, Ord, Generic, Show)

instance FromRow InternalNodeRow where
  fromRow = InternalNodeRow <$> field <*> field <*> field

instance ToRow InternalNodeRow where
  toRow (InternalNodeRow h l r) = toRow (h, l, r)

data LeafRow = LeafRow Recipient Hash TokenAmount AllocationIx
  deriving (Eq, Ord, Generic, Show)

instance FromRow LeafRow where
  fromRow = LeafRow <$> field <*> field <*> field <*> field

instance ToRow LeafRow where
  toRow (LeafRow r h a i) = toRow (r, h, a, i)

data Step = GoLeft | GoRight
  deriving (Eq, Ord, Generic, Show)

newtype TopDown step = TopDown {unTopDown :: [step]}
  deriving newtype (Eq, Ord, Show)

-- | Assuming an absolute index (starting from `0`) of a leaf it is easy to compute a path *from* the root:
-- | * Start at leaf level which is tree height.
-- | * If an index is even the branch goes down and right.
-- | * If an index is odd the branch goes down and left.
-- | * Repeat till you reach root level.
mkNodeTopDownPath :: Height -> AllocationIx -> TopDown Step
mkNodeTopDownPath (Height height) allocationIx = do
  let go 0 _ = []
      go l i = do
        let i' = i `div` 2
            l' = l - 1
        if even i
          then GoLeft : go l' i'
          else GoRight : go l' i'
  TopDown . reverse . go height . unAllocationIx $ allocationIx

-- | A bottom up path from the leaf to the root with sibling hashes:
-- | * `Left` means left sibling hash.
-- | * `Right` means right sibling hash.
data AllocationProof = AllocationProof TokenAllocation (TopDown (Either Hash Hash))
  deriving (Eq, Ord, Show)

-- | Construct membership proof for a leaf in the merkle tree:
-- | * Select leaf to get the index.
-- | * Compute the path from the index.
-- | * Traverse the path top down using the path.
-- | * The proof consists of sibling hashes.
-- | * Root and element hashes are not included in the proof.
mkAllocationProof :: Connection -> Root -> Recipient -> ExceptT WithdrawError IO AllocationProof
mkAllocationProof conn (Root rootHash height) recipient =
  liftIO (query conn "SELECT * FROM Leaf WHERE recipient = ?" (Only recipient)) >>= \case
    [LeafRow _ _ tokenAmount ix] -> do
      let path = mkNodeTopDownPath height ix
          getNode hash = do
            let q = "SELECT * FROM Node WHERE hash = ?"
            liftIO (query conn q (Only hash)) >>= \case
              [InternalNodeRow _ left right] -> do
                pure (hash, left, right)
              _ -> throwError AllocationNotFound
          go _ [] = pure [] -- Impossible case.
          go (_, leftHash, rightHash) [step] = do
            let (sibling, nodeHash) = case step of
                  GoLeft -> (Right rightHash, leftHash)
                  GoRight -> (Left leftHash, rightHash)
            if nodeHash == unusableTokenAllocationHash
              then throwError AllocationAlreadyWithdrawn
              else pure [sibling]
          go (_, leftHash, rightHash) (step : steps) = do
            let (sibling, nodeHash) = case step of
                  GoLeft -> (Right rightHash, leftHash)
                  GoRight -> (Left leftHash, rightHash)
            node <- getNode nodeHash
            (sibling :) <$> go node steps
      rootNode <- getNode rootHash
      topDownSiblingHashes <- go rootNode (unTopDown path)
      pure $ AllocationProof (TokenAllocation recipient tokenAmount) $ TopDown topDownSiblingHashes
    _ -> throwError AllocationNotFound

combineHash :: Hash -> Hash -> Hash
combineHash (Hash h1) (Hash h2) =
  Hash $ Plutus.fromBuiltin $ Validator.combineHash (Plutus.toBuiltin h1) (Plutus.toBuiltin h2)

hashTokenAllocation :: TokenAllocation -> Hash
hashTokenAllocation (TokenAllocation (Recipient addr) (TokenAmount amount)) = do
  let addr' = Plutus.PubKeyHash $ Plutus.toBuiltin addr
      amount' = Plutus.toBuiltin amount
  Hash $ Plutus.fromBuiltin $ Validator.hashTokenAllocation (addr', amount')

data WithdrawError
  = -- | Account already withdrawn.
    AllocationAlreadyWithdrawn
  | -- | Account not found in the tree.
    AllocationNotFound
  | -- | Internal error. Should never happen.
    RootMismatch
  deriving (Eq, Ord, Show)

withdraw :: Connection -> Recipient -> Root -> IO (Either WithdrawError Root)
withdraw conn recipient root@(Root rootHash height) = withTransaction conn $ runExceptT do
  (AllocationProof (TokenAllocation _ amount) (TopDown topDownProof)) <- mkAllocationProof conn root recipient
  let oldAllocation = TokenAllocation recipient amount
      insertNode hash left right = do
        execute
          conn
          "INSERT OR IGNORE INTO Node (hash, left, right) VALUES (?, ?, ?)"
          (InternalNodeRow hash left right)

      go oldSubRoot newSubRoot = \case
        [] ->
          if oldSubRoot == rootHash
            then pure (Root newSubRoot height)
            else throwError RootMismatch
        -- Check if we have leaf with the same recipient:
        Left l : p -> do
          let oldSubRoot' = combineHash l oldSubRoot
              newSubRoot' = combineHash l newSubRoot
          liftIO (insertNode newSubRoot' l newSubRoot)
          go oldSubRoot' newSubRoot' p
        Right r : p -> do
          let oldSubRoot' = combineHash oldSubRoot r
              newSubRoot' = combineHash newSubRoot r
          liftIO (insertNode newSubRoot' newSubRoot r)
          go oldSubRoot' newSubRoot' p

  let bottomUpProof = reverse topDownProof
  go (hashTokenAllocation oldAllocation) unusableTokenAllocationHash bottomUpProof

-- | Populate the merkle tree:
-- | * Sort the allocations by address
-- | * Fill the array with empty allocations up to `2^(ceil(log2(n)))`
-- | * Insert inital set of leaves into the leaves table.
-- | * Recursively:
-- |    - Given a row of children (divisible by 2)
-- |    - Grab pair of children
-- |    - Compute the hash of the pair
-- |    - Insert the node into the nodes table
-- |    - Insert the hash into the new row of children
-- |    - Repeat until there is only one hash left
populate :: Connection -> NonEmptyList.NonEmpty TokenAllocation -> IO Root
populate conn tokenAllocations = do
  let tokenAllocations' =
        sortOn (\(TokenAllocation (Recipient addr) _) -> addr) (NonEmptyList.toList tokenAllocations) <&> \tokanAllocation -> do
          let hash = hashTokenAllocation tokanAllocation
          (hash, tokanAllocation)

      totalAllocations = length tokenAllocations'
      height =
        if totalAllocations == 1
          then 1
          else ceiling (logBase (2.0 :: Float) (fromIntegral totalAllocations))
      numberOfLeaves = 2 ^ height
      leavesRow =
        tokenAllocations'
          <> replicate
            (numberOfLeaves - totalAllocations)
            (unusableTokenAllocationHash, unusableTokenAllocation)

  for_ (zip [0 ..] leavesRow) \(i, (hash, TokenAllocation recipient amount)) -> do
    execute
      conn
      "INSERT OR IGNORE INTO Leaf (recipient, hash, tokenAmount, ix) VALUES (?, ?, ?, ?)"
      (LeafRow recipient hash amount (AllocationIx i))

  let processRow [] = pure []
      processRow [_] = error "Odd number of children"
      processRow (l : r : cs) = do
        let hash = combineHash l r
        execute
          conn
          "INSERT OR IGNORE INTO Node (hash, left, right) VALUES (?, ?, ?)"
          (InternalNodeRow hash l r)
        (hash :) <$> processRow cs

      go [root] = pure root
      go row = do
        row' <- processRow row
        go row'

  Root
    <$> go (map fst leavesRow)
    <*> pure (Height height)

-- | Useful for testing on small size trees but not useful in practice:
-- | * So we load the full inner node table to memory.
-- | * And we load the full leaf table to memory.
-- | * We traverse the tree in memory.
-- |
-- | We have to do this because:
-- | * Sqlite doesn't support `IN` queries: https://github.com/nurpax/sqlite-simple/issues/91
-- | * We don't want to introduce reverse link to parent because that would make the tree "immutable".
inefficientRemainingAllocations :: Connection -> Root -> IO [TokenAllocation]
inefficientRemainingAllocations conn (Root root _) = do
  (allNodes :: [InternalNodeRow]) <-
    query_
      conn
      "SELECT * FROM Node"
  let node2children = Map.fromList $ allNodes <&> \(InternalNodeRow hash left right) -> (hash, (left, right))
  let -- If children are not present in the inner node table they are leaves.
      go parents = do
        let children = flip foldMap parents \parent -> do
              case Map.lookup parent node2children of
                Nothing -> []
                Just (left, right) -> [left, right]
        if null children
          then pure parents
          else go children
  children <- go [root]
  allLeaves <- do
    leaves <-
      query_
        conn
        "SELECT * FROM Leaf"
    pure $ Map.fromList $ leaves <&> \(LeafRow r h a _) -> (h, (r, a))

  fmap catMaybes $ for children \h ->
    if h /= unusableTokenAllocationHash
      then case Map.lookup h allLeaves of
        Nothing ->
          -- FIXME: Replace with proper error handling.
          error "Leaf not found"
        Just (r, a) -> pure $ Just $ TokenAllocation r a
      else pure Nothing
