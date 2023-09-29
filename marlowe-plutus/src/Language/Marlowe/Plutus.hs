{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}

-- Enable the following options to dump the Plutus code for the validators.
--
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:dump-pir #-}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:dump-plc #-}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:dump-uplc #-}

-- | Marlowe validators.
--
-- Module      :  Language.Marlowe.Plutus
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
module Language.Marlowe.Plutus (
  -- * Semantics Validator
  marloweValidator,
  marloweValidatorBytes,
  marloweValidatorCompiled,
  marloweValidatorHash,
  mkMarloweValidator,

  -- * Payout Validator
  rolePayoutValidator,
  rolePayoutValidatorBytes,
  rolePayoutValidatorHash,

  -- * Open role validator
  openRoleValidator,
  openRoleValidatorBytes,
  openRoleValidatorHash,

  -- * Utilities
  hashScript,
  serialiseCompiledCode,
  serialiseUPLC,
) where

import Cardano.Crypto.Hash qualified as Hash
import Codec.Serialise (serialise)
import Control.Lens (over)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Short qualified as SBS
import Data.Functor (void)
import Flat (flat)
import GHC.Generics (Generic)
import Language.Marlowe.Core.V1.Semantics as Semantics
import Language.Marlowe.Core.V1.Semantics.Types as Semantics
import Language.Marlowe.Scripts (MarloweInput, MarloweTxInput (..))
import Plutus.V1.Ledger.Address (scriptHashAddress)
import Plutus.V1.Ledger.Address qualified as Address (scriptHashAddress)
import Plutus.V1.Ledger.Api (ValidatorHash (..))
import Plutus.V1.Ledger.Value (valueOf)
import Plutus.V1.Ledger.Value qualified as Val
import Plutus.V2.Ledger.Api (
  Credential (..),
  Datum (Datum),
  DatumHash (DatumHash),
  Extended (..),
  FromData (..),
  Interval (..),
  LowerBound (..),
  POSIXTime (..),
  POSIXTimeRange,
  Redeemer (..),
  ScriptContext (ScriptContext, scriptContextPurpose, scriptContextTxInfo),
  ScriptPurpose (Spending),
  SerializedScript,
  TxInInfo (TxInInfo, txInInfoOutRef, txInInfoResolved),
  TxInfo (TxInfo, txInfoInputs, txInfoOutputs, txInfoValidRange),
  UnsafeFromData (..),
  UpperBound (..),
  Value (..),
  adaSymbol,
 )
import Plutus.V2.Ledger.Api qualified as Ledger (Address (Address))
import Plutus.V2.Ledger.Contexts (findDatum, findDatumHash, txSignedBy, valueSpent)
import Plutus.V2.Ledger.Tx (OutputDatum (OutputDatumHash), TxOut (TxOut, txOutAddress, txOutDatum, txOutValue))
import PlutusTx (CompiledCode, getPlc)
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Plugin ()
import PlutusTx.Prelude as PlutusTxPrelude hiding (traceError, traceIfFalse)
import UntypedPlutusCore (DefaultFun, DefaultUni)
import UntypedPlutusCore qualified as UPLC
import Prelude qualified as Haskell

-- Conditionally suppress traces, in order to save bytes.

#ifdef TRACE_PLUTUS

import PlutusTx.Prelude (traceError, traceIfFalse)

#else

{-# INLINABLE traceError #-}
traceError :: BuiltinString -> a
traceError _ = error ()

{-# INLINABLE traceIfFalse #-}
traceIfFalse :: BuiltinString -> a -> a
traceIfFalse _ = id

#endif

{-# INLINEABLE rolePayoutValidator #-}

-- | The Marlowe payout validator.
rolePayoutValidator :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
rolePayoutValidator =
  $$(PlutusTx.compile [||rolePayoutValidator'||])
  where
    rolePayoutValidator' :: BuiltinData -> BuiltinData -> BuiltinData -> ()
    rolePayoutValidator' d r p =
      check
        $ mkRolePayoutValidator
          (unsafeFromBuiltinData d)
          (unsafeFromBuiltinData r)
          (unsafeFromBuiltinData p)

-- | The Marlowe payout validator.
mkRolePayoutValidator
  :: (CurrencySymbol, TokenName)
  -- ^ The datum is the currency symbol and role name for the payout.
  -> ()
  -- ^ No redeemer is required.
  -> ScriptContext
  -- ^ The script context.
  -> Bool
  -- ^ Whether the transaction validated.
mkRolePayoutValidator (currency, role) _ ctx =
  -- The role token for the correct currency must be present.
  -- [Marlowe-Cardano Specification: "17. Payment authorized".]
  Val.singleton currency role 1 `Val.leq` valueSpent (scriptContextTxInfo ctx)

-- | Compute the hash of a script.
hashScript :: CompiledCode fn -> ValidatorHash
hashScript =
  ValidatorHash
    . toBuiltin
    . (Hash.hashToBytes :: Hash.Hash Hash.Blake2b_224 SBS.ShortByteString -> BS.ByteString)
    . Hash.hashWith (BS.append "\x02" . SBS.fromShort) -- For Plutus V2.
    . serialiseCompiledCode

serialiseCompiledCode :: CompiledCode a -> SBS.ShortByteString
serialiseCompiledCode = serialiseUPLC . toNameless . void . getPlc
  where
    toNameless
      :: UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
      -> UPLC.Program UPLC.DeBruijn DefaultUni DefaultFun ()
    toNameless = over UPLC.progTerm $ UPLC.termMapNames UPLC.unNameDeBruijn

-- | Turns a program's AST (most likely manually constructed)
-- into a binary format that is understood by the network and can be stored on-chain.
serialiseUPLC :: UPLC.Program UPLC.DeBruijn DefaultUni DefaultFun () -> SBS.ShortByteString
serialiseUPLC =
  SBS.toShort . BSL.toStrict . serialise . flat

-- | The hash of the Marlowe payout validator.
rolePayoutValidatorHash :: ValidatorHash
rolePayoutValidatorHash = hashScript rolePayoutValidator

-- | The serialisation of the Marlowe payout validator.
rolePayoutValidatorBytes :: SerializedScript
rolePayoutValidatorBytes = serialiseCompiledCode rolePayoutValidator

{-# INLINEABLE closeInterval #-}

-- | Convert a Plutus POSIX time range into the closed interval needed by Marlowe semantics.
closeInterval :: POSIXTimeRange -> Maybe (POSIXTime, POSIXTime)
closeInterval (Interval (LowerBound (Finite (POSIXTime l)) lc) (UpperBound (Finite (POSIXTime h)) hc)) =
  Just
    ( POSIXTime $ l + 1 - fromEnum lc -- Add one millisecond if the interval was open.
    , POSIXTime $ h - 1 + fromEnum hc -- Subtract one millisecond if the interval was open.
    )
closeInterval _ = Nothing

{-# INLINEABLE mkMarloweValidator #-}

-- | The Marlowe semantics validator.
mkMarloweValidator
  :: ValidatorHash
  -- ^ The hash of the corresponding Marlowe payout validator.
  -> MarloweData
  -- ^ The datum is the Marlowe parameters, state, and contract.
  -> MarloweInput
  -- ^ The redeemer is the list of inputs applied to the contract.
  -> ScriptContext
  -- ^ The script context.
  -> Bool
  -- ^ Whether the transaction validated.
mkMarloweValidator
  rolePayoutValidatorHash
  MarloweData{..}
  marloweTxInputs
  ctx@ScriptContext{scriptContextTxInfo} = do
    let scriptInValue = txOutValue $ txInInfoResolved ownInput
    let interval =
          -- Marlowe semantics require a closed interval, so we might adjust by one millisecond.
          case closeInterval $ txInfoValidRange scriptContextTxInfo of
            Just interval' -> interval'
            Nothing -> traceError "a"

    -- Find Contract continuation in TxInfo datums by hash or fail with error.
    let inputs = fmap marloweTxInputToInput marloweTxInputs

    {-  We do not check that a transaction contains exact input payments.
        We only require an evidence from a party, e.g. a signature for PubKey party,
        or a spend of a 'party role' token.  This gives huge flexibility by allowing
        parties to provide multiple inputs (either other contracts or P2PKH).
        Then, we check scriptOutput to be correct.
     -}
    let inputContents = fmap getInputContent inputs

    -- Check that the required signatures and role tokens are present.
    -- [Marlowe-Cardano Specification: "Constraint 14. Inputs authorized".]
    let inputsOk = allInputsAreAuthorized inputContents

    -- [Marlowe-Cardano Specification: "Constraint 5. Input value from script".]
    -- [Marlowe-Cardano Specification: "Constraint 13. Positive balances".]
    -- [Marlowe-Cardano Specification: "Constraint 19. No duplicates".]
    -- Check that the initial state obeys the Semantic's invariants.
    let preconditionsOk = checkState "i" scriptInValue marloweState

    -- [Marlowe-Cardano Specification: "Constraint 0. Input to semantics".]
    -- Package the inputs to be applied in the semantics.
    let txInput =
          TransactionInput
            { txInterval = interval
            , txInputs = inputs
            }

    -- [Marlowe-Cardano Specification: "Constraint 7. Input state".]
    -- [Marlowe-Cardano Specification: "Constraint 8. Input contract".]
    -- The semantics computation operates on the state and contract from
    -- the incoming datum.
    let computedResult = computeTransaction txInput marloweState marloweContract
    case computedResult of
      TransactionOutput{txOutPayments, txOutState, txOutContract} -> do
        -- [Marlowe-Cardano Specification: "Constraint 9. Marlowe parameters".]
        -- [Marlowe-Cardano Specification: "Constraint 10. Output state".]
        -- [Marlowe-Cardano Specification: "Constraint 11. Output contract."]
        -- The output datum maintains the parameters and uses the state
        -- and contract resulting from the semantics computation.
        let marloweData =
              MarloweData
                { marloweParams = marloweParams
                , marloweContract = txOutContract
                , marloweState = txOutState
                }

            -- Each party must receive as least as much value as the semantics specify.
            -- [Marlowe-Cardano Specification: "Constraint 15. Sufficient payment."]
            payoutsByParty = AssocMap.toList $ foldMap payoutByParty txOutPayments
            payoutsOk = payoutConstraints payoutsByParty

            checkContinuation = case txOutContract of
              -- [Marlowe-Cardano Specification: "Constraint 4. No output to script on close".]
              Close -> traceIfFalse "c" hasNoOutputToOwnScript
              _ ->
                let totalIncome = foldMap collectDeposits inputContents
                    totalPayouts = foldMap snd payoutsByParty
                    finalBalance = scriptInValue + totalIncome - totalPayouts
                 in -- [Marlowe-Cardano Specification: "Constraint 3. Single Marlowe output".]
                    -- [Marlowe-Cardano Specification: "Constraint 6. Output value to script."]
                    -- Check that the single Marlowe output has the correct datum and value.
                    checkOwnOutputConstraint marloweData finalBalance
                      -- [Marlowe-Cardano Specification: "Constraint 18. Final balance."]
                      -- [Marlowe-Cardano Specification: "Constraint 13. Positive balances".]
                      -- [Marlowe-Cardano Specification: "Constraint 19. No duplicates".]
                      -- Check that the final state obeys the Semantic's invariants.
                      && checkState "o" finalBalance txOutState
        preconditionsOk
          && inputsOk
          && payoutsOk
          && checkContinuation
          -- [Marlowe-Cardano Specification: "20. Single satisfaction".]
          -- Either there must be no payouts, or there must be no other validators.
          && traceIfFalse "z" (null payoutsByParty || noOthers)
      Error TEAmbiguousTimeIntervalError -> traceError "i"
      Error TEApplyNoMatchError -> traceError "n"
      Error (TEIntervalError (InvalidInterval _)) -> traceError "j"
      Error (TEIntervalError (IntervalInPastError _ _)) -> traceError "k"
      Error TEUselessTransaction -> traceError "u"
      Error TEHashMismatch -> traceError "m"
    where
      -- The roles currency is in the Marlowe parameters.
      MarloweParams{rolesCurrency} = marloweParams

      -- Find the input being spent by a script.
      findOwnInput :: ScriptContext -> Maybe TxInInfo
      findOwnInput ScriptContext{scriptContextTxInfo = TxInfo{txInfoInputs}, scriptContextPurpose = Spending txOutRef} =
        find (\TxInInfo{txInInfoOutRef} -> txInInfoOutRef == txOutRef) txInfoInputs
      findOwnInput _ = Nothing

      -- [Marlowe-Cardano Specification: "2. Single Marlowe script input".]
      -- The inputs being spent by this script, and whether other validators are present.
      ownInput :: TxInInfo
      noOthers :: Bool
      (ownInput@TxInInfo{txInInfoResolved = TxOut{txOutAddress = ownAddress}}, noOthers) =
        case findOwnInput ctx of
          Just ownTxInInfo -> examineScripts (sameValidatorHash ownTxInInfo) Nothing True (txInfoInputs scriptContextTxInfo)
          _ -> traceError "x" -- Input to be validated was not found.

      -- Check for the presence of multiple Marlowe validators or other Plutus validators.
      examineScripts
        :: (ValidatorHash -> Bool) -- Test for this validator.
        -> Maybe TxInInfo -- The input for this validator, if found so far.
        -> Bool -- Whether no other validator has been found so far.
        -> [TxInInfo] -- The inputs remaining to be examined.
        -> (TxInInfo, Bool) -- The input for this validator and whether no other validators are present.
        -- This validator has not been found.
      examineScripts _ Nothing _ [] = traceError "x"
      -- This validator has been found, and other validators may have been found.
      examineScripts _ (Just self) noOthers [] = (self, noOthers)
      -- Found both this validator and another script, so we short-cut.
      examineScripts _ (Just self) False _ = (self, False)
      -- Found one script.
      examineScripts f mSelf noOthers (tx@TxInInfo{txInInfoResolved = TxOut{txOutAddress = Ledger.Address (ScriptCredential vh) _}} : txs)
        -- The script is this validator.
        | f vh = case mSelf of
            -- We hadn't found it before, so we save it in `mSelf`.
            Nothing -> examineScripts f (Just tx) noOthers txs
            -- We already had found this validator before
            Just _ -> traceError "w"
        -- The script is something else, so we set `noOther` to `False`.
        | otherwise = examineScripts f mSelf False txs
      -- An input without a validator is encountered.
      examineScripts f self others (_ : txs) = examineScripts f self others txs

      -- Check if inputs are being spent from the same script.
      sameValidatorHash :: TxInInfo -> ValidatorHash -> Bool
      sameValidatorHash TxInInfo{txInInfoResolved = TxOut{txOutAddress = Ledger.Address (ScriptCredential vh1) _}} vh2 = vh1 == vh2
      sameValidatorHash _ _ = False

      -- Check a state for the correct value, positive accounts, and no duplicates.
      checkState :: BuiltinString -> Val.Value -> State -> Bool
      checkState tag expected State{..} =
        let positiveBalance :: (a, Integer) -> Bool
            positiveBalance (_, balance) = balance > 0
            noDuplicates :: (Eq k) => AssocMap.Map k v -> Bool
            noDuplicates am =
              let test [] = True -- An empty list has no duplicates.
                  test (x : xs) -- Look for a duplicate of the head in the tail.
                    | elem x xs = False -- A duplicate is present.
                    | otherwise = test xs -- Continue searching for a duplicate.
               in test $ AssocMap.keys am
         in -- [Marlowe-Cardano Specification: "Constraint 5. Input value from script".]
            -- and/or
            -- [Marlowe-Cardano Specification: "Constraint 18. Final balance."]
            traceIfFalse ("v" <> tag) (totalBalance accounts == expected)
              -- [Marlowe-Cardano Specification: "Constraint 13. Positive balances".]
              && traceIfFalse ("b" <> tag) (all positiveBalance $ AssocMap.toList accounts)
              -- [Marlowe-Cardano Specification: "Constraint 19. No duplicates".]
              && traceIfFalse ("ea" <> tag) (noDuplicates accounts)
              && traceIfFalse ("ec" <> tag) (noDuplicates choices)
              && traceIfFalse ("eb" <> tag) (noDuplicates boundValues)

      -- Look up the Datum hash for specific data.
      findDatumHash' :: (PlutusTx.ToData o) => o -> Maybe DatumHash
      findDatumHash' datum = findDatumHash (Datum $ PlutusTx.toBuiltinData datum) scriptContextTxInfo

      -- Check that the correct datum and value is being output to the script.
      checkOwnOutputConstraint :: MarloweData -> Val.Value -> Bool
      checkOwnOutputConstraint ocDatum ocValue =
        let hsh = findDatumHash' ocDatum
         in traceIfFalse "d"
              $ checkScriptOutput (==) ownAddress hsh ocValue getContinuingOutput -- "Output constraint"
      getContinuingOutput :: TxOut
      getContinuingOutput = case filter (\TxOut{txOutAddress} -> ownAddress == txOutAddress) allOutputs of
        [out] -> out
        _ -> traceError "o" -- No continuation or multiple Marlowe contract outputs is forbidden.

      -- Check that address, value, and datum match the specified.
      checkScriptOutput :: (Val.Value -> Val.Value -> Bool) -> Ledger.Address -> Maybe DatumHash -> Val.Value -> TxOut -> Bool
      checkScriptOutput comparison addr hsh value TxOut{txOutAddress, txOutValue, txOutDatum = OutputDatumHash svh} =
        txOutValue `comparison` value && hsh == Just svh && txOutAddress == addr
      checkScriptOutput _ _ _ _ _ = False

      -- Check for any output to the script address.
      hasNoOutputToOwnScript :: Bool
      hasNoOutputToOwnScript = all ((/= ownAddress) . txOutAddress) allOutputs

      -- All of the script outputs.
      allOutputs :: [TxOut]
      allOutputs = txInfoOutputs scriptContextTxInfo

      -- Check merkleization and transform transaction input to semantics input.
      marloweTxInputToInput :: MarloweTxInput -> Input
      marloweTxInputToInput (MerkleizedTxInput input hash) =
        case findDatum (DatumHash hash) scriptContextTxInfo of
          Just (Datum d) ->
            let continuation = PlutusTx.unsafeFromBuiltinData d
             in MerkleizedInput input hash continuation
          Nothing -> traceError "h"
      marloweTxInputToInput (Input input) = NormalInput input

      -- Check that inputs are authorized.
      allInputsAreAuthorized :: [InputContent] -> Bool
      allInputsAreAuthorized = all validateInputWitness
        where
          validateInputWitness :: InputContent -> Bool
          validateInputWitness input =
            case input of
              IDeposit _ party _ _ -> validatePartyWitness party -- The party must witness a deposit.
              IChoice (ChoiceId _ party) _ -> validatePartyWitness party -- The party must witness a choice.
              INotify -> True -- No witness is needed for a notify.
            where
              validatePartyWitness :: Party -> Bool
              validatePartyWitness (Address _ address) = traceIfFalse "s" $ txSignedByAddress address -- The key must have signed.
              validatePartyWitness (Role role) =
                traceIfFalse "t"
                  $ Val.singleton rolesCurrency role 1 -- The role token must be present.
                  `Val.leq` valueSpent scriptContextTxInfo

      -- Tally the deposits in the input.
      collectDeposits :: InputContent -> Val.Value
      collectDeposits (IDeposit _ _ (Token cur tok) amount)
        | amount > 0 = Val.singleton cur tok amount -- SCP-5123: Semantically negative deposits
        | otherwise = zero -- do not remove funds from the script's UTxO.
      collectDeposits _ = zero

      -- Extract the payout to a party.
      payoutByParty :: Payment -> AssocMap.Map Party Val.Value
      payoutByParty (Payment _ (Party party) (Token cur tok) amount)
        | amount > 0 = AssocMap.singleton party $ Val.singleton cur tok amount
        | otherwise = AssocMap.empty -- NOTE: Perhaps required because semantics may make zero payments
        -- (though this passes the test suite), but removing this function's
        -- guard reduces the validator size by 20 bytes.
      payoutByParty (Payment _ (Account _) _ _) = AssocMap.empty

      -- Check outgoing payments.
      payoutConstraints :: [(Party, Val.Value)] -> Bool
      payoutConstraints payoutsByParty = all payoutToTxOut payoutsByParty
        where
          payoutToTxOut :: (Party, Val.Value) -> Bool
          payoutToTxOut (party, value) = case party of
            -- [Marlowe-Cardano Specification: "Constraint 15. Sufficient Payment".]
            -- SCP-5128: Note that the payment to an address may be split into several outputs but the payment to a role must be
            -- a single output. The flexibility of multiple outputs accommodates wallet-related practicalities such as the change and
            -- the return of the role token being in separate UTxOs in situations where a contract is also paying to the address
            -- where that change and that role token are sent.
            Address _ address -> traceIfFalse "p" $ value `Val.leq` valuePaidToAddress address -- At least sufficient value paid.
            Role role ->
              let hsh = findDatumHash' (rolesCurrency, role)
                  addr = Address.scriptHashAddress rolePayoutValidatorHash
               in -- Some output must have the correct value and datum to the role-payout address.
                  traceIfFalse "r" $ any (checkScriptOutput Val.geq addr hsh value) allOutputs

      -- The key for the address must have signed.
      txSignedByAddress :: Ledger.Address -> Bool
      txSignedByAddress (Ledger.Address (PubKeyCredential pkh) _) = scriptContextTxInfo `txSignedBy` pkh
      txSignedByAddress _ = False

      -- Tally the value paid to an address.
      valuePaidToAddress :: Ledger.Address -> Val.Value
      valuePaidToAddress address = foldMap txOutValue $ filter ((== address) . txOutAddress) allOutputs

-- | The validator for Marlowe semantics.
marloweValidator :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
marloweValidator =
  let marloweValidator' :: ValidatorHash -> BuiltinData -> BuiltinData -> BuiltinData -> ()
      marloweValidator' rpvh d r p =
        check
          $ mkMarloweValidator
            rpvh
            (unsafeFromBuiltinData d)
            (unsafeFromBuiltinData r)
            (unsafeFromBuiltinData p)
   in $$(PlutusTx.compile [||marloweValidator'||])
        `PlutusTx.applyCode` PlutusTx.liftCode rolePayoutValidatorHash

marloweValidatorCompiled
  :: PlutusTx.CompiledCode (ValidatorHash -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
marloweValidatorCompiled = Haskell.undefined

--  let
--    mkUntypedMarloweValidator :: ValidatorHash -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
--    mkUntypedMarloweValidator rp = Scripts.mkUntypedValidator (mkMarloweValidator rp)
--  in
--    $$(PlutusTx.compile [|| mkUntypedMarloweValidator ||])

-- | The hash of the Marlowe semantics validator.
marloweValidatorHash :: ValidatorHash
marloweValidatorHash = hashScript marloweValidator

-- | The serialisation of the Marlowe payout validator.
marloweValidatorBytes :: SerializedScript
marloweValidatorBytes = serialiseCompiledCode marloweValidator

-- By decoding only the part of the script context I was able
-- to bring down the size of the validator from 4928 to 4540 bytes.
data SubScriptContext = SubScriptContext
  { subScriptContextTxInfo :: SubTxInfo
  , subScriptContextPurpose :: ScriptPurpose
  }
  deriving stock (Generic, Haskell.Eq, Haskell.Show)

instance Eq SubScriptContext where
  {-# INLINEABLE (==) #-}
  SubScriptContext info purpose == SubScriptContext info' purpose' = info == info' && purpose == purpose'

data SubTxInfo = SubTxInfo
  { subTxInfoInputs :: [TxInInfo]
  , subTxInfoReferenceInputs :: BuiltinData
  , subTxInfoOutputs :: BuiltinData
  , subTxInfoFee :: BuiltinData
  , subTxInfoMint :: BuiltinData
  , subTxInfoDCert :: BuiltinData
  , subTxInfoWdrl :: BuiltinData
  , subTxInfoValidRange :: BuiltinData
  , subTxInfoSignatories :: BuiltinData
  , subTxInfoRedeemers :: AssocMap.Map ScriptPurpose Redeemer
  , subTxInfoData :: BuiltinData
  , subTxInfoId :: BuiltinData
  }
  deriving stock (Generic, Haskell.Show, Haskell.Eq)

instance Eq SubTxInfo where
  {-# INLINEABLE (==) #-}
  SubTxInfo i ri o f m c w r s rs d tid == SubTxInfo i' ri' o' f' m' c' w' r' s' rs' d' tid' =
    i
      == i'
      && ri
      == ri'
      && o
      == o'
      && f
      == f'
      && m
      == m'
      && c
      == c'
      && w
      == w'
      && r
      == r'
      && s
      == s'
      && rs
      == rs'
      && d
      == d'
      && tid
      == tid'

{- Open Role validator - it releases role token(s) (you can put more coins of the same role token to it) based on a few conditions:

   1. Value should contain only min. ada and a specific role token(s).
   2. Transaction should spend Marlowe output which contains corresponding thread token (the same currency symbol as
   role and token name as defined in the inlined datum).
   3. The list of Marlowe inputs should contain at least one action either `IDeposit` or `IChoice` dedicated for the same role
   party as the locked role token.

   In the current version we assume that thread token has the same currency as the role token, even though this might actually be undesired.

   Error codes:

   "1" - Own input (never happen) or Marlowe input not found.
   "2" - Invalid own value - we expect only the role token(s) and min ADA.
   "3" - Invalid Marlowe redeemer.
   "4" - Missing thread token.
-}
mkOpenRoleValidator
  :: ValidatorHash
  -- ^ The hash of the corresponding Marlowe validator.
  -> Semantics.TokenName
  -- ^ Datum should be a thread token name.
  -> BuiltinData
  -- ^ We ignore redeemer - no need for decoding
  -> SubScriptContext
  -- ^ The script context.
  -> Bool
mkOpenRoleValidator
  marloweValidatorHash
  threadTokenName
  _
  SubScriptContext
    { subScriptContextTxInfo = SubTxInfo{subTxInfoInputs, subTxInfoRedeemers}
    , subScriptContextPurpose = Spending txOutRef
    } = do
    let marloweValidatorAddress = scriptHashAddress marloweValidatorHash
        -- Performance:
        -- In the case of three inputs `find` seems to be faster than custom single pass over the list.
        -- Inlined pattern matching over `Maybe` in both cases also seems to be faster than separate helper function.
        ownInput = case find (\TxInInfo{txInInfoOutRef} -> txInInfoOutRef == txOutRef) subTxInfoInputs of
          Just input -> input
          Nothing -> traceError "1" -- Own input not found.
        marloweInput = case find (\TxInInfo{txInInfoResolved} -> txOutAddress txInInfoResolved == marloweValidatorAddress) subTxInfoInputs of
          Just input -> input
          Nothing -> traceError "1" -- Marlowe input not found.
        TxInInfo{txInInfoResolved = TxOut{txOutValue = ownValue}} = ownInput

        -- Extract role token information from the own input `Value`.
        (currencySymbol, roleName) = do
          let valuesList = AssocMap.toList $ getValue ownValue
          -- Value should contain only min. ADA and a specific role token(s) (we can have few coins of the same role
          -- token - they are all released).
          -- Performance: `find` performs here clearly worse.
          case valuesList of
            [(possibleAdaSymbol, _), (currencySymbol, AssocMap.toList -> [(roleName, _)])]
              | possibleAdaSymbol PlutusTxPrelude.== adaSymbol -> (currencySymbol, roleName)
            [(currencySymbol, AssocMap.toList -> [(roleName, _)]), _] -> (currencySymbol, roleName)
            _ -> traceError "2" -- Invalid value - we expect only the role token(s).

        -- In order to release the role token we have to encounter an action which uses/unlocks the role.
        -- All the other actions will be checked by Marlowe validator itself.
        marloweRedeemerOk = do
          let TxInInfo{txInInfoOutRef = marloweTxOutRef} = marloweInput
              inputContentUsesRole (Semantics.IDeposit _ (Semantics.Role role) _ _) = role PlutusTxPrelude.== roleName
              inputContentUsesRole (Semantics.IChoice (Semantics.ChoiceId _ (Semantics.Role role)) _) = role PlutusTxPrelude.== roleName
              inputContentUsesRole _ = False

              inputUsesRole (MerkleizedTxInput inputContent _) = inputContentUsesRole inputContent
              inputUsesRole (Input inputContent) = inputContentUsesRole inputContent

              inputs :: MarloweInput
              inputs = case AssocMap.lookup (Spending marloweTxOutRef) subTxInfoRedeemers of
                Nothing -> traceError "3" -- Invalid Marlowe redeemer
                Just (Redeemer bytes) -> case fromBuiltinData bytes of
                  Just inputs -> inputs
                  _ -> traceError "3" -- Invalid Marlowe redeemer
          isJust $ find inputUsesRole inputs

        -- Check the Marlowe input `Value` for the thread token.
        threadTokenOk = do
          let marloweValue = txOutValue $ txInInfoResolved marloweInput
          traceIfFalse "4" (valueOf marloweValue currencySymbol threadTokenName > 0)
    marloweRedeemerOk && threadTokenOk
mkOpenRoleValidator _ _ _ _ = False

-- Copied from marlowe-cardano. This is pretty standard way to minimize size of the typed validator:
--  * Wrap validator function so it accepts raw `BuiltinData`.
--  * Create a validator which is simply typed.
--  * Create "typed by `Any` validator".
--  * Coerce it if you like. This step is not required - we only need `TypedValidator`.
openRoleValidator :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
openRoleValidator = do
  let openRoleValidator' :: ValidatorHash -> BuiltinData -> BuiltinData -> BuiltinData -> ()
      openRoleValidator' mvh d r p = PlutusTxPrelude.check $ mkOpenRoleValidator mvh (unsafeFromBuiltinData d) r (unsafeFromBuiltinData p)
  $$(PlutusTx.compile [||openRoleValidator'||])
    `PlutusTx.applyCode` PlutusTx.liftCode marloweValidatorHash

openRoleValidatorBytes :: SerializedScript
openRoleValidatorBytes = serialiseCompiledCode openRoleValidator

openRoleValidatorHash :: ValidatorHash
openRoleValidatorHash = hashScript openRoleValidator

PlutusTx.makeLift ''SubTxInfo
PlutusTx.makeIsDataIndexed ''SubTxInfo [('SubTxInfo, 0)]

PlutusTx.makeLift ''SubScriptContext
PlutusTx.makeIsDataIndexed ''SubScriptContext [('SubScriptContext, 0)]
