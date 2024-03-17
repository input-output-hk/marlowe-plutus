{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Marlowe semantics validator.
module Airdrop.TokenAllocationTree (
  createTables,
  hashAllocation,
  inefficientLeavesByRoot,
  mkAllocationProof,
  mkNodeTopDownPath,
  populate,
  withdraw,
  AllocationProof (..),
  TokenAllocation (..),
  AllocationIx (..),
  Height (..),
  Recipient (..),
  TokenAmount (..),
  TopDown (..),
  TotalNumberOfLeaves (..),
  Root (..),
  Hash (..),
  Step (..),
)
where

import qualified Airdrop.Validator as Validator
import Data.ByteString (ByteString)
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.List (sortOn)
import qualified Data.Map as Map
import Database.SQLite.Simple (Connection, FromRow, Only (..), ToRow, execute, execute_, query, query_, withTransaction)
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.FromRow (FromRow (fromRow), field)
import Database.SQLite.Simple.ToField (ToField)
import Database.SQLite.Simple.ToRow (ToRow (toRow))
import GHC.Generics (Generic)
import qualified PlutusLedgerApi.V2 as Plutus
import qualified PlutusTx.Prelude as Plutus
import Prelude

newtype Hash = Hash ByteString
  deriving newtype (Eq, Ord, Show, FromField, ToField)

newtype Height = Height Integer

data Root = Root Hash Height

newtype Recipient = Recipient ByteString
  deriving newtype (Eq, Ord, Show, FromField, ToField)

invalidRecipient :: Recipient
invalidRecipient = Recipient ""

newtype TokenAmount = TokenAmount Integer
  deriving newtype (Eq, Ord, Show, FromField, ToField)

instance Semigroup TokenAmount where
  TokenAmount a <> TokenAmount b = TokenAmount (a + b)

instance Monoid TokenAmount where
  mempty = TokenAmount 0

data TokenAllocation = TokenAllocation Recipient TokenAmount
  deriving (Eq, Ord, Generic, Show)

-- An absolute index of the allocation in the airdrop shard.
newtype AllocationIx = AllocationIx {unAllocationIx :: Integer}
  deriving newtype (Eq, Ord, Show, FromField, ToField)

-- We want to store merkle tree in a way that allows non destructive updates:
--  * Nodes point to their children.
--  * Path is easily computed from the absolute child index and the total number of leaves.
--  * The garbage collection or rebuild by the chain history fold can be done pretty easily.
--
-- We need only two tables:
-- 1. A table for merkle tree internal nodes: `Node (Hash, Left Hash, Right Hash)`.
-- 2. A table for merkle tree leaves: `Leaf (Hash, Recipient, TokenAmount, Ix)`.
--
-- Database consistency:

-- * We assume that we don't perform any deletions.

-- * To simplify the schema we don't introduce `FOREIGN KEY` constraints in internal nodes which would be nullable.

-- * We can consider a trigger which performs a check against both tables when a new row is inserted..

--
createTables :: Connection -> IO ()
createTables conn = do
  -- We use blobs as they are significantly smalle and there is no difference in performance:
  -- https://sqlite.org/forum/info/69c8cbbe2d84410f
  execute_ conn "CREATE TABLE IF NOT EXISTS Node (hash BLOB PRIMARY KEY, left BLOB, right BLOB)"
  execute_ conn "CREATE TABLE IF NOT EXISTS Leaf (hash BLOB PRIMARY KEY, recipient BLOB, tokenAmount INTEGER, ix INTEGER)"

data InternalNodeRow = InternalNodeRow Hash Hash Hash
  deriving (Eq, Ord, Generic, Show)

instance FromRow InternalNodeRow where
  fromRow = InternalNodeRow <$> field <*> field <*> field

instance ToRow InternalNodeRow where
  toRow (InternalNodeRow h l r) = toRow (h, l, r)

data LeafRow = LeafRow Hash Recipient TokenAmount AllocationIx
  deriving (Eq, Ord, Generic, Show)

instance FromRow LeafRow where
  fromRow = LeafRow <$> field <*> field <*> field <*> field

instance ToRow LeafRow where
  toRow (LeafRow h r a i) = toRow (h, r, a, i)

data Step = GoLeft | GoRight
  deriving (Eq, Ord, Generic, Show)

newtype TotalNumberOfLeaves = TotalNumberOfLeaves Integer

newtype TopDown step = TopDown {unTopDown :: [step]}
  deriving newtype (Eq, Ord, Show)

-- Assuming an absolute index (starting from `0`) of a leaf it is easy to compute a path *from* the root:

-- * If an index is even the branch goes down and right.

-- * If an index is odd the branch goes down and left.
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
mkAllocationProof :: Connection -> Root -> Recipient -> IO AllocationProof
mkAllocationProof conn (Root rootHash height) recipient =
  query conn "SELECT * FROM Leaf WHERE recipient = ?" (Only recipient) >>= \case
    [LeafRow _ _ tokenAmount ix] -> do
      let path = mkNodeTopDownPath height ix

          getNode hash = do
            let q = "SELECT * FROM Node WHERE hash = ?"
            query conn q (Only hash) >>= \case
              [InternalNodeRow _ left right] -> do
                pure (hash, left, right)
              _ -> error "Node not found"

          go _ [] = pure []
          go (_, leftHash, rightHash) [step] = do
            let sibling = case step of
                  GoLeft -> Right rightHash
                  GoRight -> Left leftHash
            pure [sibling]
          go (_, leftHash, rightHash) (step : steps) = do
            let (sibling, nodeHash) = case step of
                  GoLeft -> (Right rightHash, leftHash)
                  GoRight -> (Left leftHash, rightHash)
            node <- getNode nodeHash
            (sibling :) <$> go node steps

      rootNode <- getNode rootHash
      topDownSiblingHashes <- go rootNode (unTopDown path)
      pure $ AllocationProof (TokenAllocation recipient tokenAmount) $ TopDown topDownSiblingHashes
    _ -> error "Leaf not found"

combineHash :: Hash -> Hash -> Hash
combineHash (Hash h1) (Hash h2) =
  Hash $ Plutus.fromBuiltin $ Validator.combineHash (Plutus.toBuiltin h1) (Plutus.toBuiltin h2)

hashAllocation :: TokenAllocation -> Hash
hashAllocation (TokenAllocation (Recipient addr) (TokenAmount amount)) = do
  let addr' = Plutus.PubKeyHash $ Plutus.toBuiltin addr
      amount' = Plutus.toBuiltin amount
  Hash $ Plutus.fromBuiltin $ Validator.hashAccount (addr', amount')

data SomeNodeHash = InnerNodeHash Hash | LeafHash Hash
  deriving (Eq, Ord, Show)

assertExists :: Connection -> SomeNodeHash -> IO ()
assertExists conn (InnerNodeHash hash) = do
  query conn "SELECT COUNT(*) FROM Node WHERE hash = ?" (Only hash) >>= \case
    [[1 :: Int]] -> pure ()
    _ -> error $ "Inner node not found: " <> show hash
assertExists conn (LeafHash hash) = do
  query conn "SELECT COUNT(*) FROM Leaf WHERE hash = ?" (Only hash) >>= \case
    [[1 :: Int]] -> pure ()
    _ -> error $ "Leaf not found: " <> show hash

withdraw :: Connection -> Recipient -> Root -> IO Root
withdraw conn recipient root@(Root rootHash height) = withTransaction conn do
  (AllocationProof (TokenAllocation _ amount) (TopDown topDownProof)) <- mkAllocationProof conn root recipient
  let oldAllocation = TokenAllocation recipient amount
      newAllocation = TokenAllocation recipient mempty

      insertNode hash left right = do
        execute
          conn
          "INSERT OR IGNORE INTO Node (hash, left, right) VALUES (?, ?, ?)"
          (InternalNodeRow hash left right)

      go oldSubRoot newSubRoot = \case
        [] ->
          if oldSubRoot == rootHash
            then pure (Root newSubRoot height)
            else error "Root mismatch"
        Left l : p -> do
          let oldSubRoot' = combineHash l oldSubRoot
              newSubRoot' = combineHash l newSubRoot
          insertNode newSubRoot' l newSubRoot
          assertExists conn (InnerNodeHash oldSubRoot')
          go oldSubRoot' newSubRoot' p
        Right r : p -> do
          let oldSubRoot' = combineHash oldSubRoot r
              newSubRoot' = combineHash newSubRoot r
          insertNode newSubRoot' newSubRoot r
          assertExists conn (InnerNodeHash oldSubRoot')
          go oldSubRoot' newSubRoot' p

  -- Insert new "empty" allocation.
  execute
    conn
    "INSERT OR IGNORE INTO Leaf (hash, recipient, tokenAmount, ix) VALUES (?, ?, ?, ?)"
    (LeafRow (hashAllocation newAllocation) recipient mempty (AllocationIx 0))

  let bottomUpProof = reverse topDownProof
  case bottomUpProof of
    (leaf : _) -> do
      let leafHash = either id id leaf
      assertExists conn (LeafHash leafHash)
    _ -> error "Empty proof"
  assertExists conn (LeafHash (hashAllocation oldAllocation))
  go (hashAllocation oldAllocation) (hashAllocation newAllocation) bottomUpProof

-- | * Sort the allocations by address
-- | * Fill the array with empty allocations up to `2^(ceil(log2(n)))`
-- | * Insert inital set of leafs into the leafs table.
-- | * Recursively:
-- |    - Given a row of children (divisible by 2)
-- |    - Grab pair of children
-- |    - Compute the hash of the pair
-- |    - Insert the node into the nodes table
-- |    - Insert the hash into the new row of children
-- |    - Repeat until there is only one hash left
populate :: Connection -> [TokenAllocation] -> IO Root
populate conn tokenAllocations = do
  let tokenAllocations' =
        sortOn (\(TokenAllocation (Recipient addr) _) -> addr) tokenAllocations <&> \tokanAllocation -> do
          let hash = hashAllocation tokanAllocation
          (hash, tokanAllocation)

      emptyAllocation = TokenAllocation invalidRecipient (TokenAmount 0)
      emptyAllocationHash = hashAllocation emptyAllocation

      totalAllocations = length tokenAllocations'
      numberOfLeaves = 2 ^ (ceiling (logBase (2.0 :: Float) (fromIntegral totalAllocations)) :: Int)
      leavesRow = tokenAllocations' <> replicate (numberOfLeaves - totalAllocations) (emptyAllocationHash, emptyAllocation)

  for_ (zip [0 ..] leavesRow) \(i, (hash, TokenAllocation recipient amount)) -> do
    execute
      conn
      "INSERT OR IGNORE INTO Leaf (hash, recipient, tokenAmount, ix) VALUES (?, ?, ?, ?)"
      (LeafRow hash recipient amount (AllocationIx i))

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
    <*> pure (Height $ ceiling (logBase (2.0 :: Float) (fromIntegral numberOfLeaves)))

-- | Useful for testing on small size trees but not useful in practice:
-- | * Loads the full inner tree to memory.
-- | * Sqlite doesn't support `IN` queries: https://github.com/nurpax/sqlite-simple/issues/91-
-- | * We don't want to introduce reverse link to parent because that would make the tree "immutable".
inefficientLeavesByRoot :: Connection -> Root -> IO [(Recipient, TokenAmount, AllocationIx)]
inefficientLeavesByRoot conn (Root root _) = do
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
    pure $ Map.fromList $ leaves <&> \(LeafRow h r a i) -> (h, (r, a, i))
  pure $
    children <&> \h -> case Map.lookup h allLeaves of
      Nothing -> error "Leaf not found"
      Just (r, a, i) -> (r, a, i)
