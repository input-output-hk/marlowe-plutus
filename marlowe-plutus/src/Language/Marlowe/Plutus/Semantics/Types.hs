{- FOURMOLU_DISABLE -}
-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-----------------------------------------------------------------------------
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
-- A big hammer, but it helps.
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}

-- | Types for Marlowe semantics
module Language.Marlowe.Plutus.Semantics.Types (
  -- * Type Aliases
  AccountId,
  Accounts,
  ChoiceName,
  ChosenNum,
  Money,
  TimeInterval,
  Timeout,

  -- * Contract Types
  Action (Deposit, Choice, Notify),
  Bound (..),
  Case (Case, MerkleizedCase),
  ChoiceId (..),
  Contract (..),
  CurrencySymbol (..),
  Environment (..),
  Input (..),
  InputContent (..),
  IntervalResult (..),
  Observation (..),
  Party (..),
  Payee (..),
  State (..),
  Token (..),
  TokenName (..),
  Value (..),
  ValueId (..),

  -- * Error Types
  IntervalError (..),

  -- * Utility Functions
  emptyState,
  getAction,
  getInputContent,
  inBounds,
) where

import Control.Newtype.Generics (Newtype)
import Data.String (IsString (..))
import GHC.Generics
import Language.Marlowe.Plutus.Semantics.Types.Address
import qualified PlutusLedgerApi.V1.Value as Val
import PlutusLedgerApi.V2 (CurrencySymbol (unCurrencySymbol), POSIXTime (..), TokenName (unTokenName))
import qualified PlutusLedgerApi.V2 as Ledger (Address (..))
import PlutusTx.AssocMap (Map)
import qualified PlutusTx.AssocMap as Map
import PlutusTx.Lift (makeLift)
import PlutusTx.Prelude hiding (encodeUtf8, mapM, (<$>), (<*>), (<>))
import qualified Prelude as Haskell

#if defined(ASDATA_CASE) || defined(ASDATA_ACTION)
import Data.Data (Data)
import PlutusTx (FromData, ToData, UnsafeFromData, makeIsDataIndexed)
import PlutusTx.AsData (asData)
#else
import PlutusTx (makeIsDataIndexed)
#endif

-- | A Party to a contractt.
data Party
  = -- | Party identified by a network address.
    Address Network Ledger.Address
  | -- | Party identified by a role token name.
    Role TokenName
  deriving stock (Generic, Haskell.Eq, Haskell.Ord, Haskell.Show)

makeIsDataIndexed ''Party [('Address, 0), ('Role, 1)]

-- | A party's internal account in a contract.
type AccountId = Party

-- | A timeout in a contract.
type Timeout = POSIXTime

-- | A multi-asset value.
type Money = Val.Value

-- | The name of a choice in a contract.
type ChoiceName = BuiltinByteString

-- | A numeric choice in a contract.
type ChosenNum = Integer

-- | The time validity range for a Marlowe transaction, inclusive of both endpoints.
type TimeInterval = (POSIXTime, POSIXTime)

-- | Token - represents a currency or token, it groups
--   a pair of a currency symbol and token name.
data Token = Token CurrencySymbol TokenName
  deriving stock (Generic, Haskell.Eq, Haskell.Ord)

makeIsDataIndexed ''Token [('Token, 0)]

-- | The accounts in a contract.
type Accounts = Map (AccountId, Token) Integer

-- | Choices – of integers – are identified by ChoiceId which combines a name for
-- the choice with the Party who had made the choice.
data ChoiceId = ChoiceId BuiltinByteString Party
  deriving stock (Haskell.Show, Generic, Haskell.Eq, Haskell.Ord)

makeIsDataIndexed ''ChoiceId [('ChoiceId, 0)]

instance Haskell.Show Token where
  showsPrec p (Token cs tn) =
    Haskell.showParen
      (p Haskell.>= 11)
      (Haskell.showString $ "Token \"" Haskell.++ Haskell.show cs Haskell.++ "\" " Haskell.++ Haskell.show tn)

-- | Values, as defined using Let ar e identified by name,
--   and can be used by 'UseValue' construct.
newtype ValueId = ValueId BuiltinByteString
  deriving (IsString, Haskell.Show) via TokenName
  deriving stock (Haskell.Eq, Haskell.Ord, Generic)
  deriving anyclass (Newtype)

makeIsDataIndexed ''ValueId [('ValueId, 0)]

-- | Values include some quantities that change with time,
--   including “the time interval”, “the current balance of an account”,
--   and any choices that have already been made.
--
--   Values can also be scaled, and combined using addition, subtraction, and negation.
data Value a
  = AvailableMoney AccountId Token
  | Constant Integer
  | NegValue (Value a)
  | AddValue (Value a) (Value a)
  | SubValue (Value a) (Value a)
  | MulValue (Value a) (Value a)
  | DivValue (Value a) (Value a)
  | ChoiceValue ChoiceId
  | TimeIntervalStart
  | TimeIntervalEnd
  | UseValue ValueId
  | Cond a (Value a) (Value a)
  deriving stock (Haskell.Show, Generic, Haskell.Eq, Haskell.Ord)

makeIsDataIndexed
  ''Value
  [ ('AvailableMoney, 0)
  , ('Constant, 1)
  , ('NegValue, 2)
  , ('AddValue, 3)
  , ('SubValue, 4)
  , ('MulValue, 5)
  , ('DivValue, 6)
  , ('ChoiceValue, 7)
  , ('TimeIntervalStart, 8)
  , ('TimeIntervalEnd, 9)
  , ('UseValue, 10)
  , ('Cond, 11)
  ]

-- | Observations are Boolean values derived by comparing values,
--   and can be combined using the standard Boolean operators.
--
--   It is also possible to observe whether any choice has been made
--   (for a particular identified choice).
data Observation
  = AndObs Observation Observation
  | OrObs Observation Observation
  | NotObs Observation
  | ChoseSomething ChoiceId
  | ValueGE (Value Observation) (Value Observation)
  | ValueGT (Value Observation) (Value Observation)
  | ValueLT (Value Observation) (Value Observation)
  | ValueLE (Value Observation) (Value Observation)
  | ValueEQ (Value Observation) (Value Observation)
  | TrueObs
  | FalseObs
  deriving stock (Haskell.Show, Generic, Haskell.Eq, Haskell.Ord)

makeIsDataIndexed
  ''Observation
  [ ('AndObs, 0)
  , ('OrObs, 1)
  , ('NotObs, 2)
  , ('ChoseSomething, 3)
  , ('ValueGE, 4)
  , ('ValueGT, 5)
  , ('ValueLT, 6)
  , ('ValueLE, 7)
  , ('ValueEQ, 8)
  , ('TrueObs, 9)
  , ('FalseObs, 10)
  ]

-- | The (inclusive) bound on a choice number.
data Bound = Bound Integer Integer
  deriving stock (Haskell.Show, Generic, Haskell.Eq, Haskell.Ord)

makeIsDataIndexed ''Bound [('Bound, 0)]

#ifdef ASDATA_ACTION

asData
  [d|
    -- | Actions happen at particular points during execution.
    --   Three kinds of action are possible:
    --
    --   * A @Deposit n p v@ makes a deposit of value @v@ into account @n@ belonging to party @p@.
    --
    --   * A choice is made for a particular id with a list of bounds on the values that are acceptable.
    --     For example, @[(0, 0), (3, 5]@ offers the choice of one of 0, 3, 4 and 5.
    --
    --   * The contract is notified that a particular observation be made.
    --     Typically this would be done by one of the parties,
    --     or one of their wallets acting automatically.
    --
    --   Note that the @asData@ encompases an equivalent @makeIsDataIndexed ''Action [('Deposit, 0), ('Choice, 1), ('Notify, 2)]@.
    data Action
      = Deposit AccountId Party Token (Value Observation)
      | Choice ChoiceId [Bound]
      | Notify Observation
      deriving stock (Generic, Data)
      deriving newtype (ToData, FromData, UnsafeFromData, Haskell.Eq, Haskell.Ord, Haskell.Show)
    |]

#else

-- | Actions happen at particular points during execution.
--   Three kinds of action are possible:
--
--   * A @Deposit n p v@ makes a deposit of value @v@ into account @n@ belonging to party @p@.
--
--   * A choice is made for a particular id with a list of bounds on the values that are acceptable.
--     For example, @[(0, 0), (3, 5]@ offers the choice of one of 0, 3, 4 and 5.
--
--   * The contract is notified that a particular observation be made.
--     Typically this would be done by one of the parties,
--     or one of their wallets acting automatically.
data Action
  = Deposit AccountId Party Token (Value Observation)
  | Choice ChoiceId [Bound]
  | Notify Observation
  deriving stock (Haskell.Show, Generic, Haskell.Eq, Haskell.Ord)

makeIsDataIndexed ''Action [('Deposit, 0), ('Choice, 1), ('Notify, 2)]

#endif

-- | A payment can be made to one of the parties to the contract,
--   or to one of the accounts of the contract,
--   and this is reflected in the definition.
data Payee
  = Account AccountId
  | Party Party
  deriving stock (Haskell.Show, Generic, Haskell.Eq, Haskell.Ord)

makeIsDataIndexed ''Payee [('Account, 0), ('Party, 1)]

#ifdef ASDATA_CASE

asData
  [d|
    data Case a
      = Case Action a
      | MerkleizedCase Action BuiltinByteString
      deriving stock (Generic, Data)
      deriving newtype (ToData, FromData, UnsafeFromData, Haskell.Eq, Haskell.Ord, Haskell.Show)
    |]

#else

-- | A case is a branch of a when clause, guarded by an action.
--   The continuation of the contract may be merkleized or not.
--
--   Plutus doesn't support mutually recursive data types yet.
--   datatype Case is mutually recursive with @Contract@
data Case a
  = Case Action a
  | MerkleizedCase Action BuiltinByteString
  deriving stock (Haskell.Show, Generic, Haskell.Eq, Haskell.Ord)

makeIsDataIndexed ''Case [('Case, 0), ('MerkleizedCase, 1)]

#endif

-- | Extract the @Action@ from a @Case@.
#ifdef ASDATA_CASE
getAction :: (ToData a, UnsafeFromData a) => Case a -> Action
#else
getAction :: Case a -> Action
#endif
getAction (Case action _) = action
getAction (MerkleizedCase action _) = action
{-# INLINEABLE getAction #-}

-- | Marlowe has six ways of building contracts.
--   Five of these – 'Pay', 'Let', 'If', 'When' and 'Assert' –
--   build a complex contract from simpler contracts, and the sixth, 'Close',
--   is a simple contract.
--   At each step of execution, as well as returning a new state and continuation contract,
--   it is possible that effects – payments – and warnings can be generated too.
data Contract
  = Close
  | Pay AccountId Payee Token (Value Observation) Contract
  | If Observation Contract Contract
  | When [Case Contract] Timeout Contract
  | Let ValueId (Value Observation) Contract
  | Assert Observation Contract
  deriving stock (Haskell.Show, Generic, Haskell.Eq, Haskell.Ord)

makeIsDataIndexed
  ''Contract
  [ ('Close, 0)
  , ('Pay, 1)
  , ('If, 2)
  , ('When, 3)
  , ('Let, 4)
  , ('Assert, 5)
  ]

-- | Marlowe contract internal state. Stored in a /Datum/ of a transaction output.
data State = State
  { accounts :: Accounts
  , choices :: Map ChoiceId ChosenNum
  , boundValues :: Map ValueId Integer
  , minTime :: POSIXTime
  }
  deriving stock (Haskell.Show, Haskell.Eq, Generic)

makeIsDataIndexed ''State [('State, 0)]

-- | Execution environment. Contains a time interval of a transaction.
newtype Environment = Environment {timeInterval :: TimeInterval}
  deriving stock (Haskell.Show, Haskell.Eq, Haskell.Ord)

-- | Input for a Marlowe contract. Correspond to expected 'Action's.
data InputContent
  = IDeposit AccountId Party Token Integer
  | IChoice ChoiceId ChosenNum
  | INotify
  deriving stock (Haskell.Show, Haskell.Eq, Generic)

makeIsDataIndexed ''InputContent [('IDeposit, 0), ('IChoice, 1), ('INotify, 2)]

-- | Input to a contract, which may include the merkleized continuation
--   of the contract and its hash.
data Input
  = NormalInput InputContent
  | MerkleizedInput InputContent BuiltinByteString Contract
  deriving stock (Haskell.Show, Haskell.Eq, Generic)

makeIsDataIndexed ''Input [('NormalInput, 0), ('MerkleizedInput, 1)]

-- | Extract the content of input.
getInputContent :: Input -> InputContent
getInputContent (NormalInput inputContent) = inputContent
getInputContent (MerkleizedInput inputContent _ _) = inputContent
{-# INLINEABLE getInputContent #-}

-- | Time interval errors.
--   'InvalidInterval' means @slotStart > slotEnd@, and
--   'IntervalInPastError' means time interval is in the past, relative to the contract.
--
--   These errors should never occur, but we are always prepared.
data IntervalError
  = InvalidInterval TimeInterval
  | IntervalInPastError POSIXTime TimeInterval
  deriving stock (Haskell.Show, Generic, Haskell.Eq)

-- | Result of 'fixInterval'
data IntervalResult
  = IntervalTrimmed Environment State
  | IntervalError IntervalError
  deriving stock (Haskell.Show)

-- | Empty State for a given minimal 'POSIXTime'
emptyState :: POSIXTime -> State
emptyState sn =
  State
    { accounts = Map.empty
    , choices = Map.empty
    , boundValues = Map.empty
    , minTime = sn
    }
{-# INLINEABLE emptyState #-}

-- | Check if a 'num' is within a list of inclusive bounds.
inBounds :: ChosenNum -> [Bound] -> Bool
inBounds num = any (\(Bound l u) -> num >= l && num <= u)
{-# INLINEABLE inBounds #-}

instance Eq Party where
  {-# INLINEABLE (==) #-}
  Address n1 a1 == Address n2 a2 = n1 == n2 && a1 == a2
  Address _ _ == _ = False
  Role r1 == Role r2 = r1 == r2
  Role _ == _ = False

instance Eq ChoiceId where
  {-# INLINEABLE (==) #-}
  ChoiceId n1 p1 == ChoiceId n2 p2 = n1 == n2 && p1 == p2

instance Eq Token where
  {-# INLINEABLE (==) #-}
  Token n1 p1 == Token n2 p2 = n1 == n2 && p1 == p2

instance Eq ValueId where
  {-# INLINEABLE (==) #-}
  ValueId n1 == ValueId n2 = n1 == n2

instance Eq Payee where
  {-# INLINEABLE (==) #-}
  Account acc1 == Account acc2 = acc1 == acc2
  Account{} == _ = False
  Party p1 == Party p2 = p1 == p2
  Party{} == _ = False

instance (Eq a) => Eq (Value a) where
  {-# INLINEABLE (==) #-}
  AvailableMoney acc1 tok1 == AvailableMoney acc2 tok2 = acc1 == acc2 && tok1 == tok2
  AvailableMoney _ _ == _ = False
  Constant i1 == Constant i2 = i1 == i2
  Constant{} == _ = False
  NegValue val1 == NegValue val2 = val1 == val2
  NegValue{} == _ = False
  AddValue val1 val2 == AddValue val3 val4 = val1 == val3 && val2 == val4
  AddValue{} == _ = False
  SubValue val1 val2 == SubValue val3 val4 = val1 == val3 && val2 == val4
  SubValue{} == _ = False
  MulValue val1 val2 == MulValue val3 val4 = val1 == val3 && val2 == val4
  MulValue{} == _ = False
  DivValue val1 val2 == DivValue val3 val4 = val1 == val3 && val2 == val4
  DivValue{} == _ = False
  ChoiceValue cid1 == ChoiceValue cid2 = cid1 == cid2
  ChoiceValue{} == _ = False
  TimeIntervalStart == TimeIntervalStart = True
  TimeIntervalStart == _ = False
  TimeIntervalEnd == TimeIntervalEnd = True
  TimeIntervalEnd == _ = False
  UseValue val1 == UseValue val2 = val1 == val2
  UseValue{} == _ = False
  Cond obs1 thn1 els1 == Cond obs2 thn2 els2 = obs1 == obs2 && thn1 == thn2 && els1 == els2
  Cond{} == _ = False

instance Eq Observation where
  {-# INLINEABLE (==) #-}
  AndObs o1l o2l == AndObs o1r o2r = o1l == o1r && o2l == o2r
  AndObs{} == _ = False
  OrObs o1l o2l == OrObs o1r o2r = o1l == o1r && o2l == o2r
  OrObs{} == _ = False
  NotObs a == NotObs b = a == b
  NotObs{} == _ = False
  ChoseSomething cid1 == ChoseSomething cid2 = cid1 == cid2
  ChoseSomething _ == _ = False
  ValueGE v1l v2l == ValueGE v1r v2r = v1l == v1r && v2l == v2r
  ValueGE{} == _ = False
  ValueGT v1l v2l == ValueGT v1r v2r = v1l == v1r && v2l == v2r
  ValueGT{} == _ = False
  ValueLT v1l v2l == ValueLT v1r v2r = v1l == v1r && v2l == v2r
  ValueLT{} == _ = False
  ValueLE v1l v2l == ValueLE v1r v2r = v1l == v1r && v2l == v2r
  ValueLE{} == _ = False
  ValueEQ v1l v2l == ValueEQ v1r v2r = v1l == v1r && v2l == v2r
  ValueEQ{} == _ = False
  TrueObs == TrueObs = True
  TrueObs == _ = False
  FalseObs == FalseObs = True
  FalseObs == _ = False

instance Eq Action where
  {-# INLINEABLE (==) #-}
  Deposit acc1 party1 tok1 val1 == Deposit acc2 party2 tok2 val2 =
    acc1 == acc2 && party1 == party2 && tok1 == tok2 && val1 == val2
  Deposit{} == _ = False
  Choice cid1 bounds1 == Choice cid2 bounds2 =
    cid1
      == cid2
      && length bounds1
      == length bounds2
      && let bounds = zip bounds1 bounds2
             checkBound (Bound low1 high1, Bound low2 high2) = low1 == low2 && high1 == high2
          in all checkBound bounds
  Choice{} == _ = False
  Notify obs1 == Notify obs2 = obs1 == obs2
  Notify{} == _ = False

#ifdef ASDATA_CASE
instance (Eq a, ToData a, UnsafeFromData a) => Eq (Case a) where
#else
instance (Eq a) => Eq (Case a) where
#endif
  {-# INLINEABLE (==) #-}
  Case acl cl == Case acr cr = acl == acr && cl == cr
  Case{} == _ = False
  MerkleizedCase acl bsl == MerkleizedCase acr bsr = acl == acr && bsl == bsr
  MerkleizedCase{} == _ = False

instance Eq Contract where
  {-# INLINEABLE (==) #-}
  Close == Close = True
  Close == _ = False
  Pay acc1 payee1 tok1 value1 cont1 == Pay acc2 payee2 tok2 value2 cont2 =
    acc1 == acc2 && payee1 == payee2 && tok1 == tok2 && value1 == value2 && cont1 == cont2
  Pay{} == _ = False
  If obs1 cont1 cont2 == If obs2 cont3 cont4 =
    obs1 == obs2 && cont1 == cont3 && cont2 == cont4
  If{} == _ = False
  When cases1 timeout1 cont1 == When cases2 timeout2 cont2 =
    -- The sequences of tests are ordered for efficiency.
    timeout1
      == timeout2
      && cont1
      == cont2
      && length cases1
      == length cases2
      && and (zipWith (==) cases1 cases2)
  When{} == _ = False
  Let valId1 val1 cont1 == Let valId2 val2 cont2 =
    valId1 == valId2 && val1 == val2 && cont1 == cont2
  Let{} == _ = False
  Assert obs1 cont1 == Assert obs2 cont2 = obs1 == obs2 && cont1 == cont2
  Assert{} == _ = False

instance Eq State where
  {-# INLINEABLE (==) #-}
  l == r =
    minTime l
      == minTime r
      && accounts l
      == accounts r
      && choices l
      == choices r
      && boundValues l
      == boundValues r

-- Lifting data types to Plutus Core
makeLift ''Party
makeLift ''ChoiceId
makeLift ''Token
makeLift ''ValueId
makeLift ''Value
makeLift ''Observation
makeLift ''Bound
makeLift ''Action
makeLift ''Case
makeLift ''Payee
makeLift ''Contract
makeLift ''State
makeLift ''Environment
makeLift ''InputContent
makeLift ''Input
