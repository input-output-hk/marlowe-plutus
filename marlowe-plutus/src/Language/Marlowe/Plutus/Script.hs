{- FOURMOLU_DISABLE -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
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
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

-- | Marlowe semantics validator.
module Language.Marlowe.Plutus.Script (
  -- * Validation
  marloweValidator,
  marloweValidatorBytes,
  marloweValidatorHash,
  mkMarloweValidator,
) where

import Language.Marlowe.Plutus.Script.Types (MarloweInput, MarloweTxInput (..))
import Language.Marlowe.Plutus.Semantics as Semantics (
  MarloweData (..),
  MarloweParams (MarloweParams, rolesCurrency),
  Payment (..),
  TransactionError (
    TEAmbiguousTimeIntervalError,
    TEApplyNoMatchError,
    TEHashMismatch,
    TEIntervalError,
    TEUselessTransaction
  ),
  TransactionInput (TransactionInput, txInputs, txInterval),
  TransactionOutput (
    Error,
    TransactionOutput,
    txOutContract,
    txOutPayments,
    txOutState
  ),
  computeTransaction,
  totalBalance,
 )
import Language.Marlowe.Plutus.Semantics.Types as Semantics (
  ChoiceId (ChoiceId),
  Contract (Close),
  CurrencySymbol,
  Input (..),
  InputContent (..),
  IntervalError (IntervalInPastError, InvalidInterval),
  Party (..),
  Payee (Account, Party),
  State (..),
  Token (Token),
  TokenName,
  getInputContent,
 )
import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V2 (
  Credential (..),
  Datum (Datum),
  DatumHash (DatumHash),
  Extended (..),
  Interval (..),
  LowerBound (..),
  POSIXTime (..),
  POSIXTimeRange,
  ScriptContext (ScriptContext, scriptContextPurpose, scriptContextTxInfo),
  ScriptHash (..),
  ScriptPurpose (Spending),
  SerialisedScript,
  TxInInfo (TxInInfo, txInInfoOutRef, txInInfoResolved),
  TxInfo (TxInfo, txInfoInputs, txInfoOutputs, txInfoValidRange),
  UnsafeFromData (..),
  UpperBound (..),
  serialiseCompiledCode,
 )
import PlutusLedgerApi.V2.Contexts (findDatum, findDatumHash, txSignedBy, valueSpent)
import PlutusLedgerApi.V2.Tx (OutputDatum (OutputDatumHash), TxOut (TxOut, txOutAddress, txOutDatum, txOutValue))
import PlutusTx (CompiledCode)
import PlutusTx.Plugin ()

import PlutusLedgerApi.V1.Address as Address (scriptHashAddress)
import PlutusTx.Prelude as PlutusTxPrelude (
  AdditiveGroup ((-)),
  AdditiveMonoid (zero),
  AdditiveSemigroup ((+)),
  Bool (..),
  BuiltinData,
  BuiltinString,
  Enum (fromEnum),
  Eq (..),
  Functor (fmap),
#ifdef CHECK_POSITIVE_BALANCES
  Integer,
#endif
  Maybe (..),
  Ord ((>)),
  Semigroup ((<>)),
  all,
  any,
  check,
#if defined(CHECK_DUPLICATE_ACCOUNTS) || defined(CHECK_DUPLICATE_CHOICES) || defined(CHECK_DUPLICATE_BINDINGS)
  elem,
#endif
  filter,
  find,
  foldMap,
  null,
  otherwise,
  snd,
  toBuiltin,
  traceError,
  traceIfFalse,
  ($),
  (&&),
  (.),
  (>=),
  (/=),
  (||),
 )

import qualified Cardano.Crypto.Hash as Hash
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import qualified PlutusLedgerApi.V1.Value as Val
import qualified PlutusLedgerApi.V2 as Ledger (Address (Address))
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AssocMap
import qualified Prelude as Haskell

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
hashScript :: CompiledCode fn -> ScriptHash
hashScript =
  ScriptHash
    . toBuiltin
    . (Hash.hashToBytes :: Hash.Hash Hash.Blake2b_224 SBS.ShortByteString -> BS.ByteString)
    . Hash.hashWith (BS.append "\x02" . SBS.fromShort) -- For Plutus V2.
    . serialiseCompiledCode

-- | The hash of the Marlowe payout validator.
rolePayoutValidatorHash :: ScriptHash
rolePayoutValidatorHash = hashScript rolePayoutValidator

-- | The serialisation of the Marlowe payout validator.
rolePayoutValidatorBytes :: SerialisedScript
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
  :: ScriptHash
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
#ifdef CHECK_PRECONDITIONS
    let preconditionsOk = checkState "i" scriptInValue marloweState
#endif

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
#ifdef CHECK_PRECONDITIONS
        preconditionsOk
          && inputsOk
#else
        inputsOk
#endif
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
        :: (ScriptHash -> Bool) -- Test for this validator.
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
      sameValidatorHash :: TxInInfo -> ScriptHash -> Bool
      sameValidatorHash TxInInfo{txInInfoResolved = TxOut{txOutAddress = Ledger.Address (ScriptCredential vh1) _}} vh2 = vh1 == vh2
      sameValidatorHash _ _ = False

      -- Check a state for the correct value, positive accounts, and no duplicates.
      checkState :: BuiltinString -> Val.Value -> State -> Bool
      checkState tag expected State{..} =
        let
#ifdef CHECK_POSITIVE_BALANCES
            positiveBalance :: (a, Integer) -> Bool
            positiveBalance (_, balance) = balance > 0
#endif
#if defined(CHECK_DUPLICATE_ACCOUNTS) || defined(CHECK_DUPLICATE_CHOICES) || defined(CHECK_DUPLICATE_BINDINGS)
            noDuplicates :: (Eq k) => AssocMap.Map k v -> Bool
            noDuplicates am =
              let test [] = True -- An empty list has no duplicates.
                  test (x : xs) -- Look for a duplicate of the head in the tail.
                    | elem x xs = False -- A duplicate is present.
                    | otherwise = test xs -- Continue searching for a duplicate.
               in test $ AssocMap.keys am
#endif
         in -- [Marlowe-Cardano Specification: "Constraint 5. Input value from script".]
            -- and/or
            -- [Marlowe-Cardano Specification: "Constraint 18. Final balance."]
            traceIfFalse ("v" <> tag) (totalBalance accounts == expected)
              -- [Marlowe-Cardano Specification: "Constraint 13. Positive balances".]
#ifdef CHECK_POSITIVE_BALANCES
              && traceIfFalse ("b" <> tag) (all positiveBalance $ AssocMap.toList accounts)
#endif
              -- [Marlowe-Cardano Specification: "Constraint 19. No duplicates".]
#ifdef CHECK_DUPLICATE_ACCOUNTS
              && traceIfFalse ("ea" <> tag) (noDuplicates accounts)
#endif
#ifdef CHECK_DUPLICATE_CHOICES
              && traceIfFalse ("ec" <> tag) (noDuplicates choices)
#endif
#ifdef CHECK_DUPLICATE_BINDINGS
              && traceIfFalse ("eb" <> tag) (noDuplicates boundValues)
#endif

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
      payoutConstraints = all payoutToTxOut
        where
          payoutToTxOut :: (Party, Val.Value) -> Bool
          payoutToTxOut (party, value) = case party of
            -- [Marlowe-Cardano Specification: "Constraint 15. Sufficient Payment".]
            -- SCP-5128: Note that the payment to an address may be split into several outputs but the payment to a role must be
            -- a single output. The flexibility of multiple outputs accommodates wallet-related practicalities such as the change and
            -- the return of the role token being in separate UTxOs in situations where a contract is also paying to the address
            -- where that change and that role token are sent.
            Address _ address -> traceIfFalse "p" $ valuePaidToAddress address `geqSingleton` value -- At least sufficient value paid.
            Role role ->
              let hsh = findDatumHash' (rolesCurrency, role)
                  addr = Address.scriptHashAddress rolePayoutValidatorHash
               in -- Some output must have the correct value and datum to the role-payout address.
                  traceIfFalse "r" $ any (checkScriptOutput geqSingleton addr hsh value) allOutputs

      -- Check that a multiasset value is not less than another value, optimized for singletons.
      -- Note that the semantics of this function differ from `geq` in cases where the first
      -- argument contains negative quantities; however, this never occurs in values from UTxOs.
      geqSingleton :: Val.Value -> Val.Value -> Bool
      geqSingleton multi single =
        case Val.flattenValue single of
          [(c, t, a)] -> Val.valueOf multi c t >= a  -- The second value was a singleton.
          _ -> multi `Val.geq` single                -- The second value wasn't a singleton.

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
  let marloweValidator' :: ScriptHash -> BuiltinData -> BuiltinData -> BuiltinData -> ()
      marloweValidator' rpvh d r p =
        check
          $ mkMarloweValidator
            rpvh
            (unsafeFromBuiltinData d)
            (unsafeFromBuiltinData r)
            (unsafeFromBuiltinData p)
      errorOrApplied =
        $$(PlutusTx.compile [||marloweValidator'||])
          `PlutusTx.applyCode` PlutusTx.liftCode plcVersion100 rolePayoutValidatorHash
   in case errorOrApplied of
        Haskell.Left err -> Haskell.error $ "Application of role-payout validator hash to marlowe validator failed." <> err
        Haskell.Right applied -> applied

-- | The hash of the Marlowe semantics validator.
marloweValidatorHash :: ScriptHash
marloweValidatorHash = hashScript marloweValidator

-- | The serialisation of the Marlowe payout validator.
marloweValidatorBytes :: SerialisedScript
marloweValidatorBytes = serialiseCompiledCode marloweValidator
