{- FOURMOLU_DISABLE -}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- | Generate random transactions for Plutus tests.
module Language.Marlowe.PlutusSpec (
  spec,
  specForScript,
  ScriptsInfo (..),
) where

import Cardano.Api (SerialiseAsRawBytes (..), hashScriptDataBytes, unsafeHashableScriptData)
import Cardano.Api.Shelley (fromPlutusData)
import Codec.Serialise (serialise)
import Control.Lens (Lens', use, uses, (%=), (.=), (<>=), (<~), (^.))
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExcept)
import Control.Monad.Trans.State (StateT, execStateT)
import Control.Monad.Trans.Writer (runWriter, runWriterT)
import Data.Aeson (eitherDecodeFileStrict)
import Data.Bifunctor (Bifunctor (..), bimap, second)
import Data.Default (Default (..))
import Data.List (isSuffixOf, nub)
import Data.Maybe (fromJust, maybeToList)
import Data.SatInt (fromSatInt)
import Data.Traversable (forM)
import Language.Marlowe.Core.V1.Merkle (MerkleizedContract (..), deepMerkleize, merkleizeInputs)
import Language.Marlowe.Core.V1.Semantics (
  MarloweData (..),
  MarloweParams (..),
  Payment (Payment),
  TransactionInput (..),
  TransactionOutput (..),
  computeTransaction,
  paymentMoney,
  totalBalance,
 )
import Language.Marlowe.Core.V1.Semantics.Types (
  ChoiceId (ChoiceId),
  Contract (..),
  Input (..),
  InputContent (IChoice, IDeposit),
  Party (Role),
  Payee (Party),
  State (accounts),
  Token (Token),
  getInputContent,
 )
import Language.Marlowe.Scripts.Types (MarloweInput, MarloweTxInput (..))
import Paths_marlowe_plutus (getDataDir)
import PlutusLedgerApi.Common.Versions (futurePV)
import PlutusLedgerApi.V1 (BuiltinByteString, Data, toBuiltin)
import PlutusLedgerApi.V1.Address (scriptHashAddress, toPubKeyHash)
import PlutusLedgerApi.V1.Value (flattenValue, gt, valueOf)
import PlutusLedgerApi.V2 (
  Address (Address),
  Credential (..),
  CurrencySymbol,
  Data (..),
  Datum (..),
  DatumHash (..),
  EvaluationContext,
  ExBudget (..),
  ExCPU (..),
  ExMemory (..),
  Extended (Finite, NegInf, PosInf),
  Interval (Interval),
  LogOutput,
  LowerBound (LowerBound),
  OutputDatum (..),
  PubKeyHash,
  Redeemer (..),
  ScriptContext (..),
  ScriptHash,
  ScriptPurpose (Spending),
  SerialisedScript,
  ToData (..),
  TokenName,
  TxInInfo (..),
  TxInfo (..),
  TxOut (..),
  UpperBound (UpperBound),
  Value,
  VerboseMode (Verbose),
  adaSymbol,
  adaToken,
  deserialiseScript,
  evaluateScriptCounting,
  fromData,
  mkEvaluationContext,
  singleton,
  toData,
 )
import PlutusTx.These (These (..))
import Spec.Marlowe.Plutus.Arbitrary ()
import Spec.Marlowe.Plutus.Lens ((<><~))
import Spec.Marlowe.Plutus.Types (
  PayoutTransaction (..),
  PlutusTransaction (..),
  SemanticsTransaction (..),
  amount,
  datum,
  infoData,
  infoFee,
  infoInputs,
  infoOutputs,
  infoSignatories,
  infoValidRange,
  input,
  inputContract,
  inputState,
  marloweParamsPayout,
  redeemer,
  role,
  scriptPurpose,
 )
import Spec.Marlowe.Reference (ReferencePath (..), arbitraryReferenceTransaction)
import Spec.Marlowe.Semantics.Arbitrary (arbitraryGoldenTransaction, arbitraryPositiveInteger)
import Spec.Marlowe.Semantics.Golden (GoldenTransaction)
import System.Directory (listDirectory)
import System.FilePath ((<.>), (</>))
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec (Spec, describe, it, runIO, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (
  Arbitrary (arbitrary),
  Gen,
  Property,
  Testable (property),
  chooseInteger,
#if defined(CHECK_PRECONDITIONS) && (defined(CHECK_DUPLICATE_ACCOUNTS) || defined(CHECK_DUPLICATE_CHOICES) || defined(CHECK_DUPLICATE_BINDINGS))
  elements,
#endif
  forAll,
  frequency,
  listOf,
  listOf1,
  oneof,
  shuffle,
  suchThat,
  (===),
 )

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Language.Marlowe.Core.V1.Semantics as M (MarloweData (marloweParams))
import qualified Language.Marlowe.Core.V1.Semantics.Types as M (
  Party (Address),
#if defined(CHECK_PRECONDITIONS) && (defined(CHECK_DUPLICATE_ACCOUNTS) || defined(CHECK_DUPLICATE_CHOICES) || defined(CHECK_DUPLICATE_BINDINGS))
  State (..)
#endif
 )
import qualified Language.Marlowe.Plutus.RolePayout as Official (
  rolePayoutValidatorBytes,
  rolePayoutValidatorHash,
 )
import qualified Language.Marlowe.Plutus.Semantics as Official (
  marloweValidatorBytes,
  marloweValidatorHash,
 )
import qualified PlutusLedgerApi.V1.Value as V (adaSymbol, adaToken, singleton)
import qualified PlutusTx.AssocMap as AM (
#if defined(CHECK_PRECONDITIONS) && (defined(CHECK_DUPLICATE_ACCOUNTS) || defined(CHECK_DUPLICATE_CHOICES) || defined(CHECK_DUPLICATE_BINDINGS))
  Map,
#endif
  fromList,
#if defined(CHECK_PRECONDITIONS) && defined(CHECK_POSITIVE_BALANCES)
  insert,
#endif
#if defined(CHECK_PRECONDITIONS) && (defined(CHECK_DUPLICATE_ACCOUNTS) || defined(CHECK_DUPLICATE_CHOICES) || defined(CHECK_DUPLICATE_BINDINGS))
  keys,
  null,
#endif
  toList,
 )
import qualified Spec.Marlowe.Plutus.Types as PC

checkPlutusLog :: Bool
maxMarloweValidatorSize :: Int
#ifdef TRACE_PLUTUS
maxMarloweValidatorSize = 11_249
checkPlutusLog = True
#else
maxMarloweValidatorSize = 10_779
checkPlutusLog = False
#endif

data ScriptsInfo = ScriptsInfo
  { semanticsValidatorBytes :: !SerialisedScript
  , semanticsValidatorHash :: !ScriptHash
  , semanticsAddress :: !Address
  , payoutValidatorBytes :: !SerialisedScript
  , payoutValidatorHash :: !ScriptHash
  , payoutAddress :: !Address
  }

instance Default ScriptsInfo where
  def =
    ScriptsInfo
      { semanticsValidatorBytes = Official.marloweValidatorBytes
      , semanticsValidatorHash = Official.marloweValidatorHash
      , semanticsAddress = scriptHashAddress Official.marloweValidatorHash
      , payoutValidatorBytes = Official.rolePayoutValidatorBytes
      , payoutValidatorHash = Official.rolePayoutValidatorHash
      , payoutAddress = scriptHashAddress Official.rolePayoutValidatorHash
      }

spec :: Spec
spec = specForScript def

specForScript :: ScriptsInfo -> Spec
specForScript scripts@ScriptsInfo{semanticsValidatorHash, payoutValidatorHash} = do
  referencePaths <- runIO readReferencePaths
  describe "Marlowe validator" do
    it "Should be a reasonable size" $ marloweValidatorSize scripts
    describe "Valid transactions" do
      prop "Noiseless" $ checkSemanticsTransaction scripts mempty referencePaths noModify noModify noVeto True False True
      prop "Noisy" $ checkSemanticsTransaction scripts mempty referencePaths noModify noModify noVeto True True True
    prop "Constraint 2. Single Marlowe script input" $ checkDoubleInput scripts referencePaths
    prop "Constraint 3. Single Marlowe output" $ checkMultipleOutput scripts referencePaths
    prop "Constraint 4. No output to script on close" $ checkCloseOutput scripts referencePaths
#ifdef CHECK_PRECONDITIONS
    prop "Constraint 5. Input value from script" $ checkValueInput scripts referencePaths
#endif
    prop "Constraint 6. Output value to script" $ checkValueOutput scripts referencePaths
    prop "Constraint 9. Marlowe parameters" $ checkParamsOutput scripts referencePaths
    prop "Constraint 10. Output state" $ checkStateOutput scripts referencePaths
    prop "Constraint 11. Output contract" $ checkContractOutput scripts referencePaths
    describe "Constraint 12. Merkleized continuations" do
      prop "Valid merkleization" $ checkMerkleization scripts referencePaths True
      prop "Invalid merkleization" $ checkMerkleization scripts referencePaths False
#if defined(CHECK_PRECONDITIONS) && defined(CHECK_POSITIVE_BALANCES)
    prop "Constraint 13. Positive balances" $ checkPositiveAccounts scripts referencePaths
#endif
    prop "Constraint 14. Inputs authorized" $ checkAuthorization scripts referencePaths
    prop "Constraint 15. Sufficient payment" $ checkPayment scripts referencePaths
    prop "Constraint 18. Final balance" $ checkOutputConsistency scripts referencePaths
#if defined(CHECK_PRECONDITIONS) && (defined(CHECK_DUPLICATE_ACCOUNTS) || defined(CHECK_DUPLICATE_CHOICES) || defined(CHECK_DUPLICATE_BINDINGS))
    prop "Constraint 19. No duplicates" $ checkInputDuplicates scripts referencePaths
#endif
    prop "Constraint 20. Single satisfaction" $ checkOtherValidators scripts referencePaths
    prop "Hash golden test" $
      checkValidatorHash
        semanticsValidatorHash
        -- DO NOT ALTER THE FOLLOWING VALUE UNLESS YOU ARE COMMITTING
        -- APPROVED CHANGES TO MARLOWE'S SEMANTICS VALIDATOR. THIS HASH
        -- HAS IMPLICATIONS FOR VERSIONING, AUDIT, AND CONTRACT DISCOVERY.
        ( if checkPlutusLog
            then "f015a6a380ee5fe8a7e86110031ae9a54f7594cb6ec9ed7302964327"
            else "6027a8010c555a4dd6b08882b899f4b3167c6e4524047132202dd984"
        )
  describe "Payout validator" do
    describe "Valid transactions" do
      prop "Noiseless" $ checkPayoutTransaction scripts noModify noModify noVeto True False
      prop "Noisy" $ checkPayoutTransaction scripts noModify noModify noVeto True True
    describe "Constraint 17. Payment authorized" do
      prop "Invalid authorization for withdrawal" $ checkWithdrawal scripts True
      prop "Missing authorized withdrawal" $ checkWithdrawal scripts False
    prop "Hash golden test" $
      checkValidatorHash
        payoutValidatorHash
        -- DO NOT ALTER THE FOLLOWING VALUE UNLESS YOU ARE COMMITTING
        -- APPROVED CHANGES TO MARLOWE'S ROLE VALIDATOR. THIS HASH HAS
        -- IMPLICATIONS FOR VERSIONING, AUDIT, AND CONTRACT DISCOVERY.
        ( if checkPlutusLog
            then "7f01c268cb6c400315d6d5b9c28da380bdd4d39682d5373fae9c22a8"
            else "fdade3b86107bc715037b468574dd8d3f884a0da8c9956086b9a1a51"
        )

-- | Test that the untyped validator is not too large.
marloweValidatorSize :: ScriptsInfo -> IO ()
marloweValidatorSize ScriptsInfo{semanticsValidatorBytes} = do
  let vsize = SBS.length semanticsValidatorBytes
  vsize `shouldSatisfy` (<= maxMarloweValidatorSize)

-- | An arbitrary Plutus transaction.
type ArbitraryTransaction p a = StateT (PlutusTransaction p) Gen a

-- | Create a transaction generator for spending, with empty or default values.
bareSpending :: a -> Gen (PlutusTransaction a)
bareSpending p =
  PlutusTransaction p (Datum $ toBuiltinData ()) (Redeemer $ toBuiltinData ())
    <$> ( ScriptContext
            <$> ( TxInfo
                    mempty
                    mempty
                    mempty
                    mempty
                    mempty
                    mempty
                    mempty
                    (Interval (LowerBound NegInf False) (UpperBound PosInf True))
                    mempty
                    mempty
                    mempty
                    <$> arbitrary
                )
            <*> (Spending <$> arbitrary)
        )

-- | Create a Marlowe semantics transaction, with mostly empty and default values.
bareSemanticsTransaction
  :: GoldenTransaction
  -> Gen (PlutusTransaction SemanticsTransaction)
bareSemanticsTransaction (_state, _contract, _input, _output) =
  do
    rolesCurrency <- arbitrary
    let _params = MarloweParams{..}
    bareSpending SemanticsTransaction{..}

-- | Make the datum for a Marlowe semantics transaction.
makeSemanticsDatum
  :: MarloweParams
  -> State
  -> Contract
  -> Datum
makeSemanticsDatum params state contract = Datum . toBuiltinData $ MarloweData params state contract

-- | Make the redeemer for a Marlowe semantics transaction.
makeSemanticsRedeemer
  :: MarloweInput
  -> Redeemer
makeSemanticsRedeemer = Redeemer . toBuiltinData

-- | Convert Marlowe input to Marlowe transaction input.
inputToMarloweTxInput
  :: Input
  -> (MarloweTxInput, [(DatumHash, Datum)])
inputToMarloweTxInput (NormalInput content) = (Input content, [])
inputToMarloweTxInput (MerkleizedInput content hash contract) = (MerkleizedTxInput content hash, [(DatumHash hash, Datum $ toBuiltinData contract)])

-- | Create script input for a Marlowe semantics transaction.
makeScriptInput :: ScriptsInfo -> ArbitraryTransaction SemanticsTransaction (TxInInfo, [(DatumHash, Datum)])
makeScriptInput ScriptsInfo{semanticsAddress} =
  do
    inValue <- inputState `uses` totalValue
    inDatum <- use datum
    let inDatumHash = DatumHash $ dataHash inDatum
    (,pure (inDatumHash, inDatum))
      . flip TxInInfo (TxOut semanticsAddress inValue (OutputDatumHash inDatumHash) Nothing)
      <$> lift arbitrary

-- | Total the value in a Marlowe state.
totalValue :: State -> Value
totalValue = foldMap (\((_, Token c n), i) -> V.singleton c n i) . AM.toList . accounts

-- | Create deposit inputs for a Marlowe semantics transaction.
makeDeposit
  :: Input
  -> ArbitraryTransaction SemanticsTransaction [TxInInfo]
makeDeposit input' =
  do
    ref <- lift arbitrary
    address' <- lift $ arbitrary `suchThat` notScriptAddress
    pure $
      case getInputContent input' of
        IDeposit _ (M.Address _ address) (Token c n) i ->
          if i > 0
            then pure . TxInInfo ref $ TxOut address (V.singleton c n i) NoOutputDatum Nothing
            else mempty
        IDeposit _ (Role _) (Token c n) i ->
          if i > 0
            then pure . TxInInfo ref $ TxOut address' (V.singleton c n i) NoOutputDatum Nothing
            else mempty
        _ -> mempty

-- | Create role input for a Marlowe semantics transaction.
makeRoleIn
  :: Input
  -> ArbitraryTransaction SemanticsTransaction [TxInInfo]
makeRoleIn input' =
  do
    MarloweParams currencySymbol <- use PC.marloweParams
    ref <- lift arbitrary
    address <- lift $ arbitrary `suchThat` notScriptAddress
    pure $
      case getInputContent input' of
        IDeposit _ (Role role') _ _ -> pure . TxInInfo ref $ TxOut address (V.singleton currencySymbol role' 1) NoOutputDatum Nothing
        IChoice (ChoiceId _ (Role role')) _ -> pure . TxInInfo ref $ TxOut address (V.singleton currencySymbol role' 1) NoOutputDatum Nothing
        _ -> mempty

-- | Create script continuing output for a Marlowe semantics transaction.
makeScriptOutput :: ScriptsInfo -> ArbitraryTransaction SemanticsTransaction ([TxOut], [(DatumHash, Datum)])
makeScriptOutput ScriptsInfo{semanticsAddress} =
  do
    params <- use PC.marloweParams
    outState <- PC.output `uses` txOutState
    outContract <- PC.output `uses` txOutContract
    let outDatum = Datum . toBuiltinData $ MarloweData params outState outContract
        outDatumHash = DatumHash $ dataHash outDatum
    pure $
      unzip
        [ ( TxOut semanticsAddress (totalValue outState) (OutputDatumHash outDatumHash) Nothing
          , (outDatumHash, outDatum)
          )
        | outContract /= Close
        ]

-- | Create role output for a Marlowe semantics transaction.
makeRoleOut
  :: TxInInfo
  -> ArbitraryTransaction SemanticsTransaction TxOut
makeRoleOut (TxInInfo _ (TxOut _ token _ _)) =
  TxOut <$> lift arbitrary <*> pure token <*> pure NoOutputDatum <*> pure Nothing

-- | Create a payment for a Marlowe semantics transaction.
makePayment
  :: ScriptsInfo
  -> CurrencySymbol
  -> Payment
  -> ArbitraryTransaction SemanticsTransaction ([TxOut], [(DatumHash, Datum)])
makePayment _ _ payment@(Payment _ (Party (M.Address _ address)) _ _) =
  pure
    ( pure $ TxOut address (paymentMoney payment) NoOutputDatum Nothing
    , mempty
    )
makePayment ScriptsInfo{payoutAddress} currencySymbol payment@(Payment _ (Party (Role role')) _ _) =
  do
    let roleDatum = Datum $ toBuiltinData (currencySymbol, role')
        roleDatumHash = DatumHash $ dataHash roleDatum
    pure
      ( pure $ TxOut payoutAddress (paymentMoney payment) (OutputDatumHash roleDatumHash) Nothing
      , pure (roleDatumHash, roleDatum)
      )
makePayment _ _ _ = pure (mempty, mempty)

-- | Create a deposit or choice signatory for a Marlowe semantics transaction.
makeActionSignatory
  :: Input
  -> [PubKeyHash]
makeActionSignatory input' =
  case getInputContent input' of
    IDeposit _ (M.Address _ (Address (PubKeyCredential pkh) _)) _ _ -> pure pkh
    IChoice (ChoiceId _ (M.Address _ (Address (PubKeyCredential pkh) _))) _ -> pure pkh
    _ -> mempty

-- | Create a spending signatory for a Marlowe semantics transaction.
makeSpendSignatory
  :: TxInInfo
  -> [PubKeyHash]
makeSpendSignatory (TxInInfo _ (TxOut (Address (PubKeyCredential pkh) _) _ _ _)) = pure pkh
makeSpendSignatory _ = mempty

-- | Combine payments with the same address and hash.
consolidatePayments :: [TxOut] -> [TxOut]
consolidatePayments ps =
  let extractAddressHash (TxOut address _ hash _) = (address, hash)
      extractValue (TxOut _ value _ _) = value
   in [ TxOut address value hash Nothing
      | ah@(address, hash) <- nub $ extractAddressHash <$> ps
      , let value = foldMap extractValue $ filter ((== ah) . extractAddressHash) ps
      ]

-- | Generate a valid Marlowe semantics transaction.
validSemanticsTransaction
  :: ScriptsInfo
  -> Bool
  -- ^ Whether to add noise to the script context.
  -> ArbitraryTransaction SemanticsTransaction ()
  -- ^ The generator.
validSemanticsTransaction scripts noisy =
  do
    -- The datum is `MarloweData`.
    datum <~ makeSemanticsDatum <$> use PC.marloweParams <*> use inputState <*> use inputContract

    -- The redeemer is `MarloweInput`, but we also track the merkleizations.
    (marloweInput, merkleizations) <- input `uses` (second mconcat . unzip . fmap inputToMarloweTxInput . txInputs)
    redeemer .= makeSemanticsRedeemer marloweInput
    infoData <>= AM.fromList merkleizations

    -- Add the spending from the script.
    (inScript, inData) <- makeScriptInput scripts
    infoInputs <>= [inScript]
    infoData <>= AM.fromList inData
    scriptPurpose .= Spending (txInInfoOutRef inScript)

    -- Add the role inputs.
    roleInputs <- fmap concat . mapM makeRoleIn . txInputs =<< use input
    infoInputs <>= roleInputs

    -- Add the deposits.
    infoInputs <><~ (fmap concat . mapM makeDeposit . txInputs =<< use input)

    -- Add the script output.
    (outScript, outData) <- makeScriptOutput scripts
    infoOutputs <>= outScript
    infoData <>= AM.fromList outData

    -- Add the role outputs.
    infoOutputs <><~ mapM makeRoleOut roleInputs

    MarloweParams currencySymbol <- use PC.marloweParams
    -- Add the payments.
    (payments, paymentData) <-
      fmap (bimap mconcat mconcat . unzip) . mapM (makePayment scripts currencySymbol) =<< (PC.output `uses` txOutPayments)
    infoOutputs <>= consolidatePayments payments
    infoData <>= AM.fromList paymentData

    -- Add the signatories.
    actionSignatories <- concatMap makeActionSignatory <$> input `uses` txInputs
    spendSignatories <- concatMap makeSpendSignatory <$> use infoInputs
    infoSignatories <>= nub (actionSignatories <> spendSignatories)

    -- Set the validity interval.
    interval <- input `uses` txInterval
    infoValidRange .= Interval (LowerBound (Finite $ fst interval) True) (UpperBound (Finite $ snd interval) True)

    -- Set the fee.
    infoFee <~ V.singleton V.adaSymbol V.adaToken <$> lift arbitraryPositiveInteger

    -- Add noise.
    when noisy addNoise

    -- Shuffle.
    shuffleTransaction

-- | Generate an arbitrary, valid Marlowe semantics transaction: datum, redeemer, and script context.
arbitrarySemanticsTransaction
  :: ScriptsInfo
  -- ^ The scripts being tested.
  -> [ReferencePath]
  -- ^ The reference execution paths from which to choose.
  -> ArbitraryTransaction SemanticsTransaction ()
  -- ^ Modifications to make before building the valid transaction.
  -> ArbitraryTransaction SemanticsTransaction ()
  -- ^ Modifications to make after building the valid transaction.
  -> Bool
  -- ^ Whether to add noise to the script context.
  -> Bool
  -- ^ Whether to allow merkleization.
  -> Gen (PlutusTransaction SemanticsTransaction)
  -- ^ The generator.
arbitrarySemanticsTransaction scripts referencePaths modifyBefore modifyAfter noisy allowMerkleization =
  do
    golden <-
      frequency
        [ (1, arbitraryGoldenTransaction allowMerkleization) -- Manually vetted transactions.
        , (5, arbitraryReferenceTransaction referencePaths) -- Transactions generated using `getAllInputs` and `computeTransaction`.
        ]
    start <- bareSemanticsTransaction golden
    (modifyBefore >> validSemanticsTransaction scripts noisy >> modifyAfter)
      `execStateT` start

-- | Create a Marlowe payout transaction, with mostly empty and default values.
barePayoutTransaction :: Gen (PlutusTransaction PayoutTransaction)
barePayoutTransaction =
  do
    rolesCurrency <- arbitrary
    let _params' = MarloweParams{..}
    _role <- arbitrary
    _amount <- arbitrary `suchThat` (`gt` mempty)
    bareSpending PayoutTransaction{..}

-- | Create a script input for a Marlowe payout transaction.
makePayoutIn :: ScriptsInfo -> ArbitraryTransaction PayoutTransaction (TxInInfo, (DatumHash, Datum))
makePayoutIn ScriptsInfo{payoutAddress} =
  do
    txInInfoOutRef <- lift arbitrary
    inDatum <- ((Datum . toBuiltinData) .) . (,) <$> marloweParamsPayout `uses` rolesCurrency <*> use role
    let inDatumHash = DatumHash $ dataHash inDatum
    txInInfoResolved <- TxOut payoutAddress <$> use amount <*> pure (OutputDatumHash inDatumHash) <*> pure Nothing
    pure (TxInInfo{..}, (inDatumHash, inDatum))

-- | Create a role input for a Marlowe payout transaction.
makePayoutRoleIn :: ArbitraryTransaction PayoutTransaction TxInInfo
makePayoutRoleIn =
  do
    ref <- lift arbitrary
    address <- lift $ arbitrary `suchThat` notScriptAddress
    value <- V.singleton <$> marloweParamsPayout `uses` rolesCurrency <*> use role <*> pure 1
    pure
      . TxInInfo ref
      $ TxOut address value NoOutputDatum Nothing

-- | Create a payment output for a Marlowe payout transaction.
makePayoutOut :: ArbitraryTransaction PayoutTransaction TxOut
makePayoutOut =
  TxOut
    <$> lift arbitrary
    <*> use amount
    <*> pure NoOutputDatum
    <*> pure Nothing

-- | Create a role output for a Marlowe payout transaction.
makePayoutRoleOut :: ArbitraryTransaction PayoutTransaction TxOut
makePayoutRoleOut =
  do
    address <- lift arbitrary
    value <- V.singleton <$> marloweParamsPayout `uses` rolesCurrency <*> use role <*> pure 1
    pure $
      TxOut address value NoOutputDatum Nothing

-- | Create a spending signatory for a Marlowe payout transaction.
makePayoutSignatory
  :: TxInInfo
  -> [PubKeyHash]
makePayoutSignatory (TxInInfo _ (TxOut (Address (PubKeyCredential pkh) _) _ _ _)) = pure pkh
makePayoutSignatory _ = mempty

-- | Generate a valid Marlowe payout transaction.
validPayoutTransaction
  :: ScriptsInfo
  -- ^ The scripts being tests.
  -> Bool
  -- ^ Whether to add noise to the script context.
  -> ArbitraryTransaction PayoutTransaction ()
  -- ^ The generator.
validPayoutTransaction scripts noisy =
  do
    -- Add the script input.
    (inScript, inData@(_, inDatum)) <- makePayoutIn scripts
    infoInputs <>= [inScript]
    infoData <>= AM.fromList [inData]
    scriptPurpose .= Spending (txInInfoOutRef inScript)

    -- The datum is the currency symbol and role name.
    datum .= inDatum

    -- The redeemer is unit.
    redeemer .= Redeemer (toBuiltinData ())

    -- Add the role input.
    infoInputs <><~ (pure <$> makePayoutRoleIn)

    -- Add the pay output.
    infoOutputs <><~ (pure <$> makePayoutOut)

    -- Add the role output.
    infoOutputs <><~ (pure <$> makePayoutRoleOut)

    -- Add the signatories.
    infoSignatories <><~ (infoInputs `uses` concatMap makePayoutSignatory)

    -- Set the fee.
    infoFee <~ V.singleton V.adaSymbol V.adaToken <$> lift arbitraryPositiveInteger

    -- Add noise.
    when noisy addNoise'

    -- Shuffle.
    shuffleTransaction

-- | Check that an address is not for a script.
notScriptAddress :: Address -> Bool
notScriptAddress (Address (ScriptCredential _) _) = False
notScriptAddress _ = True

-- | Check that an input is not from a script.
notScriptTxIn :: TxInInfo -> Bool
notScriptTxIn = not . isScriptTxIn

-- | Check that an input is from a script.
isScriptTxIn :: TxInInfo -> Bool
isScriptTxIn TxInInfo{txInInfoResolved = TxOut{txOutAddress = Address (ScriptCredential _) _}} = True
isScriptTxIn _ = False

-- | Add noise to the inputs, outputs, and data in a Plutus transaction.
addNoise :: ArbitraryTransaction SemanticsTransaction ()
addNoise =
  do
    let isPayout (Payment _ (Party _) _ i) = i > 0
        isPayout _ = False
    hasPayments <- any isPayout . txOutPayments <$> use PC.output
    let arbitraryInput =
          if hasPayments
            then arbitrary `suchThat` notScriptTxIn
            else arbitrary
    infoInputs <><~ lift (listOf arbitraryInput `suchThat` ((< 5) . length))
    infoOutputs <><~ lift (arbitrary `suchThat` ((< 5) . length))
    infoData <><~ lift (fmap AM.fromList $ arbitrary `suchThat` ((< 5) . length))

-- | Add noise to the inputs, outputs, and data in a Plutus transaction.
addNoise' :: ArbitraryTransaction PayoutTransaction ()
addNoise' =
  do
    infoInputs <><~ lift (arbitrary `suchThat` ((< 5) . length))
    infoOutputs <><~ lift (arbitrary `suchThat` ((< 5) . length))
    infoData <><~ lift (fmap AM.fromList $ arbitrary `suchThat` ((< 5) . length))

-- | Shuffle the order of inputs, outputs, data, and signatories in a Plutus transaction.
shuffleTransaction :: ArbitraryTransaction a ()
shuffleTransaction =
  do
    let go :: Lens' (PlutusTransaction a) [b] -> ArbitraryTransaction a ()
        go field = field <~ (lift . shuffle =<< use field)
    go infoInputs
    go infoOutputs
    go infoSignatories
    infoData <~ (lift . fmap AM.fromList . shuffle . AM.toList =<< use infoData)

-- | Merkleize a transaction.
merkleize :: ArbitraryTransaction SemanticsTransaction ()
merkleize =
  do
    -- Fetch the original state, contract, and inputs.
    state <- use inputState
    contract <- use inputContract
    inputs <- use input
    -- Merkleize the contract and the input.
    let (mcContract, mcContinuations) = runWriter $ deepMerkleize contract
        inputs' = either error id $ merkleizeInputs MerkleizedContract{..} state inputs
    -- Update the contract, inputs, and outputs.
    inputContract .= mcContract
    input .= inputs'
    PC.output .= computeTransaction inputs' state mcContract

-- | Generate an arbitrary, valid Marlowe payout transaction: datum, redeemer, and script context.
arbitraryPayoutTransaction
  :: ScriptsInfo
  -- ^ The scripts being tested.
  -> ArbitraryTransaction PayoutTransaction ()
  -- ^ Modifications to make before building the valid transaction.
  -> ArbitraryTransaction PayoutTransaction ()
  -- ^ Modifications to make after building the valid transaction.
  -> Bool
  -- ^ Whether to add noise to the script context.
  -> Gen (PlutusTransaction PayoutTransaction)
  -- ^ The generator.
arbitraryPayoutTransaction scripts modifyBefore modifyAfter noisy =
  do
    start <- barePayoutTransaction
    (modifyBefore >> validPayoutTransaction scripts noisy >> modifyAfter)
      `execStateT` start

-- | Do not modify a Plutus transaction.
noModify :: ArbitraryTransaction a ()
noModify = pure ()

-- | Do not eliminate generated Plutus transactions.
noVeto :: PlutusTransaction a -> Bool
noVeto = const True

dataHash :: (ToData a) => a -> BuiltinByteString
dataHash = toBuiltin . serialiseToRawBytes . hashScriptDataBytes . unsafeHashableScriptData . fromPlutusData . toData

-- | Check that a semantics transaction succeeds.
checkSemanticsTransaction
  :: ScriptsInfo
  -- ^ The scripts being tested.
  -> LogOutput
  -- ^ At least one of these required log messages must be reported if the validation fails.
  -> [ReferencePath]
  -- ^ The reference execution paths from which to choose.
  -> ArbitraryTransaction SemanticsTransaction ()
  -- ^ Modifications to make before building the valid transaction.
  -> ArbitraryTransaction SemanticsTransaction ()
  -- ^ Modifications to make after building the valid transaction.
  -> (PlutusTransaction SemanticsTransaction -> Bool)
  -- ^ Whether to discard the transaction from the testing.
  -> Bool
  -- ^ Whether the transaction should test as valid.
  -> Bool
  -- ^ Whether to add noise to the script context.
  -> Bool
  -- ^ Whether to allow merkleization.
  -> Property
  -- ^ The test property.
checkSemanticsTransaction scripts requiredLog referencePaths modifyBefore modifyAfter condition valid noisy allowMerkleization =
  property
    . forAll
      ( arbitrarySemanticsTransaction scripts referencePaths modifyBefore modifyAfter noisy allowMerkleization
          `suchThat` condition
      )
    $ \PlutusTransaction{..} ->
      case evaluateSemantics scripts (toData _datum) (toData _redeemer) (toData _scriptContext) of
        This e -> not valid || error (show e)
        These e l -> not valid && matchesPlutusLog l || error (show e <> ": " <> show l)
        That _ -> valid
  where
    matchesPlutusLog l = not checkPlutusLog || any (`elem` l) requiredLog

-- | Check that a payout transaction succeeds.
checkPayoutTransaction
  :: ScriptsInfo
  -- ^ The scripts being tested.
  -> ArbitraryTransaction PayoutTransaction ()
  -- ^ Modifications to make before building the valid transaction.
  -> ArbitraryTransaction PayoutTransaction ()
  -- ^ Modifications to make after building the valid transaction.
  -> (PlutusTransaction PayoutTransaction -> Bool)
  -- ^ Whether to discard the transaction from the testing.
  -> Bool
  -- ^ Whether the transaction should test as valid.
  -> Bool
  -- ^ Whether to add noise to the script context.
  -> Property
  -- ^ The test property.
checkPayoutTransaction scripts modifyBefore modifyAfter condition valid noisy =
  property
    . forAll (arbitraryPayoutTransaction scripts modifyBefore modifyAfter noisy `suchThat` condition)
    $ \PlutusTransaction{..} ->
      case evaluatePayout scripts (toData _datum) (toData _redeemer) (toData _scriptContext) of
        This e -> not valid || error (show e)
        These e l -> not valid || error (show e <> ": " <> show l)
        That _ -> valid

-- | Check that validation fails if two Marlowe scripts are run.
checkDoubleInput :: ScriptsInfo -> [ReferencePath] -> Property
checkDoubleInput scripts@ScriptsInfo{semanticsAddress} referencePaths =
  let modifyAfter =
        do
          -- Create a random datum.
          inDatum <- lift arbitrary
          let inDatumHash = DatumHash $ dataHash inDatum
          -- Create a random input to the script.
          inScript <-
            TxInInfo
              <$> lift arbitrary
              <*> (TxOut semanticsAddress <$> lift arbitrary <*> pure (OutputDatumHash inDatumHash) <*> pure Nothing)
          -- Add a second script input.
          infoInputs <>= [inScript]
          -- Add the new datum and its hash.
          infoData <>= AM.fromList [(inDatumHash, inDatum)]
          shuffleTransaction
   in checkSemanticsTransaction scripts ["w"] referencePaths noModify modifyAfter noVeto False False False

-- | Split a value in half.
splitValue :: Value -> [Value]
splitValue value =
  concat
    [ [ singleton c n half
      , singleton c n (i - half)
      ]
    | (c, n, i) <- flattenValue value
    , let half = i `div` 2
    ]

-- | Ensure that the contract closes the contract.
doesClose :: PlutusTransaction SemanticsTransaction -> Bool
doesClose = (== Close) . txOutContract . (^. PC.output)

-- | Ensure that the transaction does not close the contract.
notCloses :: PlutusTransaction SemanticsTransaction -> Bool
notCloses = not . doesClose

-- | Ensure that the contract makes payments.
hasPayouts :: PlutusTransaction SemanticsTransaction -> Bool
hasPayouts =
  let isPayout (Payment _ (Party _) _ i) = i > 0
      isPayout _ = False
   in any isPayout . txOutPayments . (^. PC.output)

-- | Check that validation fails if there is more than one Marlowe output.
checkMultipleOutput :: ScriptsInfo -> [ReferencePath] -> Property
checkMultipleOutput scripts@ScriptsInfo{semanticsAddress} referencePaths =
  let modifyAfter =
        do
          let -- Split a script output into two equal ones.
              splitOwnOutput txOut@(TxOut address value datum' _)
                | address == semanticsAddress = flip (TxOut address) datum' <$> splitValue value <*> pure Nothing
                | otherwise = pure txOut
          -- Update the outputs with the split script output.
          infoOutputs %= concatMap splitOwnOutput
          shuffleTransaction
   in checkSemanticsTransaction scripts ["o"] referencePaths noModify modifyAfter notCloses False False False

-- | Check that validation fails if there is one Marlowe output upon close.
checkCloseOutput :: ScriptsInfo -> [ReferencePath] -> Property
checkCloseOutput scripts@ScriptsInfo{semanticsAddress} referencePaths =
  let modifyAfter =
        do
          let -- Match the script input.
              matchOwnInput (TxInInfo _ (TxOut address _ _ _)) = address == semanticsAddress
          -- Find the script input.
          inScript <- infoInputs `uses` filter matchOwnInput
          -- Add a clone of the script input as output.
          infoOutputs <>= (txInInfoResolved <$> inScript)
          shuffleTransaction
   in checkSemanticsTransaction scripts ["c"] referencePaths noModify modifyAfter doesClose False False False

#ifdef CHECK_PRECONDITIONS
-- | Check that value input to a script matches its input state.
checkValueInput :: ScriptsInfo -> [ReferencePath] -> Property
checkValueInput scripts@ScriptsInfo{semanticsAddress} referencePaths =
  let modifyAfter =
        do
          let -- Add one lovelace to the input to the script.
              incrementOwnInput txInInfo@(TxInInfo _ txOut@(TxOut address value _ _))
                | address == semanticsAddress =
                    txInInfo{txInInfoResolved = txOut{txOutValue = value <> singleton adaSymbol adaToken 1}}
                | otherwise = txInInfo
          -- Update the inputs with the incremented script input.
          infoInputs %= fmap incrementOwnInput
   in checkSemanticsTransaction scripts ["vi"] referencePaths noModify modifyAfter noVeto False False False
#endif

-- | Check that value output to a script matches its expectation.
checkValueOutput :: ScriptsInfo -> [ReferencePath] -> Property
checkValueOutput scripts@ScriptsInfo{semanticsAddress} referencePaths =
  let modifyAfter =
        do
          delta <- lift $ oneof [chooseInteger (-5, -1), chooseInteger (1, 5), arbitrary `suchThat` (/= 0)] -- Ensure small non-zero integers.
          let -- Add or subtract some lovelace to the output to the script.
              incrementOwnOutput txOut@(TxOut address value _ _)
                | address == semanticsAddress = txOut{txOutValue = value <> singleton adaSymbol adaToken delta}
                | otherwise = txOut
          -- Update the outputs with the incremented script output.
          infoOutputs %= fmap incrementOwnOutput
   in checkSemanticsTransaction scripts ["d"] referencePaths noModify modifyAfter notCloses False False False

-- | Check the consistency of the output value with the output state.
checkOutputConsistency :: ScriptsInfo -> [ReferencePath] -> Property
checkOutputConsistency scripts@ScriptsInfo{semanticsAddress} referencePaths =
  property
    . forAll (arbitrarySemanticsTransaction scripts referencePaths noModify noModify False True)
    $ \tx ->
      let findOwnOutput (TxOut address value _ _)
            | address == semanticsAddress = value
            | otherwise = mempty
          outValue = foldMap findOwnOutput $ tx ^. infoOutputs
          finalBalance = totalBalance . accounts . txOutState $ tx ^. PC.output
          -- There is really no way to provoke this invalidity in a manner that isn't covered by other tests.
          valid = outValue == finalBalance
       in checkSemanticsTransaction scripts [] referencePaths noModify noModify notCloses valid False False

#if defined(CHECK_PRECONDITIONS) && (defined(CHECK_DUPLICATE_ACCOUNTS) || defined(CHECK_DUPLICATE_CHOICES) || defined(CHECK_DUPLICATE_BINDINGS))
-- | Add a duplicate entry to an association list.
addDuplicate :: (Arbitrary v) => AM.Map k v -> Gen (AM.Map k v)
addDuplicate am =
  do
    let am' = AM.toList am
    key <- elements $ fst <$> am'
    value <- arbitrary
    AM.fromList <$> shuffle ((key, value) : am')
#endif

#if defined(CHECK_PRECONDITIONS) && (defined(CHECK_DUPLICATE_ACCOUNTS) || defined(CHECK_DUPLICATE_CHOICES) || defined(CHECK_DUPLICATE_BINDINGS))
-- | Check for the detection of duplicates in input state
checkInputDuplicates :: ScriptsInfo -> [ReferencePath] -> Property
checkInputDuplicates scripts referencePaths =
  let hasDuplicates tx =
        let hasDuplicate am = length (AM.keys am) /= length (nub $ AM.keys am)
            M.State{..} = tx ^. inputState
         in hasDuplicate accounts
              || hasDuplicate choices
              || hasDuplicate boundValues
      makeDuplicates am =
        if AM.null am
          then pure am
          else oneof [pure am, addDuplicate am]
      modifyBefore =
        do
          M.State{..} <- use inputState
#ifdef CHECK_DUPLICATE_ACCOUNTS
          accounts' <- lift $ makeDuplicates accounts
#else
          let accounts' = accounts
#endif
#ifdef CHECK_DUPLICATE_CHOICES
          choices' <- lift $ makeDuplicates choices
#else
          let choices' = choices
#endif
#ifdef CHECK_DUPLICATE_BINDINGS
          boundValues' <- lift $ makeDuplicates boundValues
#else
          let boundValues' = boundValues
#endif
          inputState <~ pure (M.State accounts' choices' boundValues' minTime)
   in checkSemanticsTransaction
        scripts
        ["bi", "eai", "ebi", "eci", "n"]
        referencePaths
        modifyBefore
        noModify
        hasDuplicates
        False
        False
        False
#endif

-- | Check that output datum to a script matches its semantic output.
checkDatumOutput :: ScriptsInfo -> [ReferencePath] -> (MarloweData -> Gen MarloweData) -> Property
checkDatumOutput scripts referencePaths perturb =
  let modifyAfter =
        do
          -- Find the existing Marlowe data output.
          marloweData <- MarloweData <$> use PC.marloweParams <*> PC.output `uses` txOutState <*> PC.output `uses` txOutContract
          -- Compute its hash.
          let outDatumHash = DatumHash $ dataHash marloweData
          -- Modify the original datum.
          outDatum' <- fmap (Datum . toBuiltinData) . lift $ perturb marloweData
          -- Let
          let -- Replace an output datum with the modification.
              perturbOwnOutputDatum pair@(h, _)
                | h == outDatumHash = (h, outDatum')
                | otherwise = pair
          -- Update the data with the modification.
          infoData %= AM.fromList . fmap perturbOwnOutputDatum . AM.toList
   in checkSemanticsTransaction scripts ["d"] referencePaths noModify modifyAfter notCloses False False False

-- | Check that other validators are forbidden during payments.
checkOtherValidators :: ScriptsInfo -> [ReferencePath] -> Property
checkOtherValidators scripts referencePaths =
  let modifyAfter =
        -- Add an extra script input.
        infoInputs <><~ lift (listOf1 $ makeScriptTxIn =<< arbitrary)
   in checkSemanticsTransaction scripts ["z"] referencePaths noModify modifyAfter hasPayouts False False False

makeScriptTxIn :: TxInInfo -> Gen TxInInfo
makeScriptTxIn (TxInInfo outRef out) = TxInInfo outRef <$> makeScriptTxOut out

makeScriptTxOut :: TxOut -> Gen TxOut
makeScriptTxOut out = do
  address' <- Address <$> (ScriptCredential <$> arbitrary) <*> arbitrary
  pure $ out{txOutAddress = address'}

-- | Check that parameters in the datum are not changed by the transaction.
checkParamsOutput :: ScriptsInfo -> [ReferencePath] -> Property
checkParamsOutput scripts referencePaths =
  checkDatumOutput scripts referencePaths $
    \marloweData ->
      do
        -- Replace the output parameters with a random one.
        let old = M.marloweParams marloweData
        new <- arbitrary `suchThat` (/= old)
        pure $ marloweData{M.marloweParams = new}

-- | Check that state output to a script matches its semantic output.
checkStateOutput :: ScriptsInfo -> [ReferencePath] -> Property
checkStateOutput scripts referencePaths =
  checkDatumOutput scripts referencePaths $
    \marloweData ->
      do
        -- Replace the output state with a random one.
        let old = marloweState marloweData
        new <- arbitrary `suchThat` (/= old)
        pure $ marloweData{marloweState = new}

-- | Check that contract output to a script matches its semantic output.
checkContractOutput :: ScriptsInfo -> [ReferencePath] -> Property
checkContractOutput scripts referencePaths =
  checkDatumOutput scripts referencePaths $
    \marloweData ->
      do
        -- Replace the output contract with a random one.
        let old = marloweContract marloweData
        new <- arbitrary `suchThat` (/= old)
        pure $ marloweData{marloweContract = new}

-- | Check that the input contract is merkleized.
hasMerkleizedInput :: PlutusTransaction SemanticsTransaction -> Bool
hasMerkleizedInput =
  let isMerkleized NormalInput{} = False
      isMerkleized MerkleizedInput{} = True
   in any isMerkleized . txInputs . (^. input)

-- | Check than an invalid merkleization is rejected.
checkMerkleization :: ScriptsInfo -> [ReferencePath] -> Bool -> Property
checkMerkleization scripts referencePaths valid =
  let -- Merkleized the contract and its input.
      modifyBefore = merkleize
      -- Extract the merkle hash, if any.
      merkleHash (NormalInput _) = mempty
      merkleHash (MerkleizedInput _ hash _) = pure $ DatumHash hash
      -- Modify the contract if requested.
      modifyAfter =
        if valid
          then pure ()
          else do
            -- Remove the merkleized continuation datums for the input.
            hashes <- input `uses` (concatMap merkleHash . txInputs)
            infoData %= (AM.fromList . filter ((`notElem` hashes) . fst) . AM.toList)
   in checkSemanticsTransaction scripts ["h"] referencePaths modifyBefore modifyAfter hasMerkleizedInput valid False False

#if defined(CHECK_PRECONDITIONS) && defined(CHECK_POSITIVE_BALANCES)
-- | Check that non-positive accounts are rejected.
checkPositiveAccounts :: ScriptsInfo -> [ReferencePath] -> Property
checkPositiveAccounts scripts referencePaths =
  let modifyBefore =
        do
          -- Create a random non-positive entry for the accounts.
          account <- lift arbitrary
          token <- lift arbitrary
          amount' <- (1 -) <$> lift arbitraryPositiveInteger
          -- Add the non-positive entry to the accounts.
          inputState %= (\state -> state{accounts = AM.insert (account, token) amount' $ accounts state})
   in checkSemanticsTransaction scripts ["bi"] referencePaths modifyBefore noModify noVeto False False False
#endif

-- | Compute the authorization for an input.
authorizer :: Input -> ([PubKeyHash], [TokenName])
authorizer (NormalInput (IDeposit _ (M.Address _ address) _ _)) = (maybeToList $ toPubKeyHash address, mempty)
authorizer (NormalInput (IDeposit _ (Role role') _ _)) = (mempty, pure role')
authorizer (NormalInput (IChoice (ChoiceId _ (M.Address _ address)) _)) = (maybeToList $ toPubKeyHash address, mempty)
authorizer (NormalInput (IChoice (ChoiceId _ (Role role')) _)) = (mempty, pure role')
authorizer (MerkleizedInput (IDeposit _ (M.Address _ address) _ _) _ _) = (maybeToList $ toPubKeyHash address, mempty)
authorizer (MerkleizedInput (IDeposit _ (Role role') _ _) _ _) = (mempty, pure role')
authorizer (MerkleizedInput (IChoice (ChoiceId _ (M.Address _ address)) _) _ _) = (maybeToList $ toPubKeyHash address, mempty)
authorizer (MerkleizedInput (IChoice (ChoiceId _ (Role role')) _) _ _) = (mempty, pure role')
authorizer _ = (mempty, mempty)

-- | Determine whether there are any authorizations in the transaction.
hasAuthorizations :: PlutusTransaction SemanticsTransaction -> Bool
hasAuthorizations = (/= ([], [])) . bimap concat concat . unzip . fmap authorizer . txInputs . (^. input)

-- | Delete only the first matching value in a list.
deleteFirst
  :: (a -> Bool)
  -- ^ The condition for deleting an element.
  -> [a]
  -- ^ The list.
  -> [a]
  -- ^ The list with the first matching element removed.
deleteFirst f z =
  case break f z of
    (x, []) -> x
    (x, y) -> x <> tail y

-- | Check that a missing authorization causes failure.
checkAuthorization :: ScriptsInfo -> [ReferencePath] -> Property
checkAuthorization scripts referencePaths =
  let modifyAfter =
        do
          currency <- PC.marloweParams `uses` rolesCurrency
          -- Determine the authorizations.
          (pkhs, roles) <- input `uses` (bimap concat concat . unzip . fmap authorizer . txInputs)
          let -- Determine whether a role token is present.
              matchRole TxInInfo{txInInfoResolved = TxOut{txOutValue}} = any (\role' -> valueOf txOutValue currency role' > 0) roles
              -- Determine whether a PKH authorization is present.
              matchPkh = (`elem` pkhs)
          -- Remove the first role token from the input.
          infoInputs %= deleteFirst matchRole
          -- Remove the first PKH signatory.
          infoSignatories %= deleteFirst matchPkh
   in checkSemanticsTransaction scripts ["s", "t"] referencePaths noModify modifyAfter hasAuthorizations False False False

-- | Determine whether there are any external payments in a transaction.
hasExternalPayments :: PlutusTransaction SemanticsTransaction -> Bool
hasExternalPayments = any externalPayment . txOutPayments . (^. PC.output)

-- | Determine whether a payment is external.
externalPayment :: Payment -> Bool
externalPayment (Payment _ (Party _) _ q) = q > 0
externalPayment _ = False

-- | Decrement the value of each token by one.
decrementValue :: Value -> Value
decrementValue = foldMap (\(c, n, i) -> singleton c n (i - 1)) . flattenValue

-- | Check that an insufficient payment causes failure.
checkPayment :: ScriptsInfo -> [ReferencePath] -> Property
checkPayment scripts@ScriptsInfo{payoutAddress} referencePaths =
  let modifyAfter =
        do
          let -- Decrement a payment by one unit.
              decrementPayment txOut@(TxOut address value (OutputDatumHash _) _)
                | address == payoutAddress =
                    txOut{txOutValue = decrementValue value}
                | otherwise = txOut
              decrementPayment txOut@(TxOut (Address (PubKeyCredential _) _) value NoOutputDatum _) = txOut{txOutValue = decrementValue value}
              decrementPayment txOut = txOut
          -- Update the outputs.
          infoOutputs %= fmap decrementPayment
   in checkSemanticsTransaction scripts ["p", "r"] referencePaths noModify modifyAfter hasExternalPayments False False False

-- | Remove a role input UTxOs from the transaction.
removeRoleIn :: ArbitraryTransaction PayoutTransaction ()
removeRoleIn =
  do
    -- Determine the roles currency and name.
    currency <- marloweParamsPayout `uses` rolesCurrency
    name <- use role
    let -- Determine if the input has the role token.
        notMatch (TxInInfo _ (TxOut _ value _ _)) = valueOf value currency name == 0
    -- Update the transaction inputs
    infoInputs %= filter notMatch

-- | Change the role name in an input UTxO from the transaction.
mutateRoleIn :: ArbitraryTransaction PayoutTransaction ()
mutateRoleIn =
  do
    -- Determine the roles currency and name.
    currency <- marloweParamsPayout `uses` rolesCurrency
    name <- use role
    -- Randomly choose a different name.
    name' <- lift $ arbitrary `suchThat` (/= name)
    let -- Mutate the roles currency.
        mutate txIn@(TxInInfo _ (TxOut _ value _ _)) =
          if valueOf value currency name /= 0
            then txIn{txInInfoResolved = (txInInfoResolved txIn){txOutValue = singleton currency name' 1}}
            else txIn
    -- Update the transaction inputs
    infoInputs %= fmap mutate

-- | Check that an invalid withdrawal transaction fails.
checkWithdrawal
  :: ScriptsInfo
  -> Bool
  -> Property
checkWithdrawal scripts mutate =
  checkPayoutTransaction
    scripts
    noModify
    (if mutate then mutateRoleIn else removeRoleIn)
    noVeto
    False
    False

-- | Check that a validator hash is correct.
checkValidatorHash
  :: ScriptHash
  -> ScriptHash
  -> Property
checkValidatorHash actual reference =
  property $ actual === reference

referenceFolder :: FilePath
referenceFolder = "reference" </> "data"

readReferencePaths :: IO [ReferencePath]
readReferencePaths =
  do
    pathFiles <- fmap (referenceFolder </>) . filter (".paths" `isSuffixOf`) <$> listDirectory referenceFolder
    fmap concat
      . forM pathFiles
      $ \pathFile ->
        eitherDecodeFileStrict pathFile
          >>= \case
            Right paths -> pure $ filter (not . null . transactions) paths
            Left msg -> error $ "Failed parsing " <> pathFile <> ": " <> msg <> "."

{-# NOINLINE unsafeDumpBenchmark #-}
-- Dump data files for benchmarking Plutus execution cost.
unsafeDumpBenchmark
  :: FilePath
  -- ^ Name of folder the benchmarks.
  -> Data
  -- ^ The datum.
  -> Data
  -- ^ The redeemer.
  -> Data
  -- ^ The script context.
  -> ExBudget
  -- ^ The Plutus execution cost.
  -> a
  -- ^ A value.
  -> a
  -- ^ The same value.
unsafeDumpBenchmark folder d r c ExBudget{..} x =
  unsafePerformIO $ -- ☹
    do
      let i = txInfoId . scriptContextTxInfo . fromJust $ fromData c
          ExCPU cpu = exBudgetCPU
          ExMemory memory = exBudgetMemory
          result =
            Constr
              0
              [ d
              , r
              , c
              , I $ fromSatInt cpu
              , I $ fromSatInt memory
              ]
          payload = serialise result
      folder' <- (</> folder) <$> getDataDir
      LBS.writeFile
        (folder' </> show i <.> "benchmark")
        payload
      pure x

-- | Dump benchmarking files.
dumpBenchmarks :: Bool
dumpBenchmarks = False

-- | Check the Plutus execution budget.
enforceBudget :: Bool
enforceBudget = False

-- | Run the Plutus evaluator on the Marlowe semantics validator.
evaluateSemantics
  :: ScriptsInfo
  -- ^ The scripts being tested.
  -> Data
  -- ^ The datum.
  -> Data
  -- ^ The redeemer.
  -> Data
  -- ^ The script context.
  -> These String LogOutput
  -- ^ The result.
evaluateSemantics ScriptsInfo{semanticsValidatorBytes} d r c =
  case deserialiseScript futurePV semanticsValidatorBytes of
    Left message -> This $ show message
    Right validator ->
      case evaluationContext of
        Left message -> This message
        Right ec -> case evaluateScriptCounting futurePV Verbose ec validator [d, r, c] of
          (logOutput, Right ex@ExBudget{..}) ->
            ( if dumpBenchmarks
                then unsafeDumpBenchmark "semantics" d r c ex
                else id
            )
              $ if enforceBudget && (exBudgetCPU > 10_000_000_000 || exBudgetMemory > 14_000_000)
                then These ("Exceeded Plutus budget: " <> show ex) logOutput
                else That logOutput
          (logOutput, Left message) -> These (show message) logOutput

-- | Run the Plutus evaluator on the Marlowe payout validator.
evaluatePayout
  :: ScriptsInfo
  -- ^ The scripts being tested.
  -> Data
  -- ^ The datum.
  -> Data
  -- ^ The redeemer.
  -> Data
  -- ^ The script context.
  -> These String LogOutput
  -- ^ The result.
evaluatePayout ScriptsInfo{payoutValidatorBytes} d r c =
  case deserialiseScript futurePV payoutValidatorBytes of
    Left message -> This $ show message
    Right validator ->
      case evaluationContext of
        Left message -> This message
        Right ec -> case evaluateScriptCounting futurePV Verbose ec validator [d, r, c] of
          (logOutput, Right ex) ->
            ( if dumpBenchmarks
                then unsafeDumpBenchmark "rolepayout" d r c ex
                else id
            )
              $ That logOutput
          (logOutput, Left message) -> These (show message) logOutput

-- | Build an evaluation context.
evaluationContext :: Either String EvaluationContext
evaluationContext = bimap show fst . runExcept . runWriterT . mkEvaluationContext $ snd <$> costModel

-- | A default cost model for Plutus.
costModel :: [(String, Integer)]
costModel =
  [ ("addInteger-cpu-arguments-intercept", 205_665)
  , ("addInteger-cpu-arguments-slope", 812)
  , ("addInteger-memory-arguments-intercept", 1)
  , ("addInteger-memory-arguments-slope", 1)
  , ("appendByteString-cpu-arguments-intercept", 1_000)
  , ("appendByteString-cpu-arguments-slope", 571)
  , ("appendByteString-memory-arguments-intercept", 0)
  , ("appendByteString-memory-arguments-slope", 1)
  , ("appendString-cpu-arguments-intercept", 1_000)
  , ("appendString-cpu-arguments-slope", 24_177)
  , ("appendString-memory-arguments-intercept", 4)
  , ("appendString-memory-arguments-slope", 1)
  , ("bData-cpu-arguments", 1_000)
  , ("bData-memory-arguments", 32)
  , ("blake2b_256-cpu-arguments-intercept", 117_366)
  , ("blake2b_256-cpu-arguments-slope", 10_475)
  , ("blake2b_256-memory-arguments", 4)
  , ("cekApplyCost-exBudgetCPU", 23_000)
  , ("cekApplyCost-exBudgetMemory", 100)
  , ("cekBuiltinCost-exBudgetCPU", 23_000)
  , ("cekBuiltinCost-exBudgetMemory", 100)
  , ("cekConstCost-exBudgetCPU", 23_000)
  , ("cekConstCost-exBudgetMemory", 100)
  , ("cekDelayCost-exBudgetCPU", 23_000)
  , ("cekDelayCost-exBudgetMemory", 100)
  , ("cekForceCost-exBudgetCPU", 23_000)
  , ("cekForceCost-exBudgetMemory", 100)
  , ("cekLamCost-exBudgetCPU", 23_000)
  , ("cekLamCost-exBudgetMemory", 100)
  , ("cekStartupCost-exBudgetCPU", 100)
  , ("cekStartupCost-exBudgetMemory", 100)
  , ("cekVarCost-exBudgetCPU", 23_000)
  , ("cekVarCost-exBudgetMemory", 100)
  , ("chooseData-cpu-arguments", 19_537)
  , ("chooseData-memory-arguments", 32)
  , ("chooseList-cpu-arguments", 175_354)
  , ("chooseList-memory-arguments", 32)
  , ("chooseUnit-cpu-arguments", 46_417)
  , ("chooseUnit-memory-arguments", 4)
  , ("consByteString-cpu-arguments-intercept", 221_973)
  , ("consByteString-cpu-arguments-slope", 511)
  , ("consByteString-memory-arguments-intercept", 0)
  , ("consByteString-memory-arguments-slope", 1)
  , ("constrData-cpu-arguments", 89_141)
  , ("constrData-memory-arguments", 32)
  , ("decodeUtf8-cpu-arguments-intercept", 497_525)
  , ("decodeUtf8-cpu-arguments-slope", 14_068)
  , ("decodeUtf8-memory-arguments-intercept", 4)
  , ("decodeUtf8-memory-arguments-slope", 2)
  , ("divideInteger-cpu-arguments-constant", 196_500)
  , ("divideInteger-cpu-arguments-model-arguments-intercept", 453_240)
  , ("divideInteger-cpu-arguments-model-arguments-slope", 220)
  , ("divideInteger-memory-arguments-intercept", 0)
  , ("divideInteger-memory-arguments-minimum", 1)
  , ("divideInteger-memory-arguments-slope", 1)
  , ("encodeUtf8-cpu-arguments-intercept", 1_000)
  , ("encodeUtf8-cpu-arguments-slope", 28_662)
  , ("encodeUtf8-memory-arguments-intercept", 4)
  , ("encodeUtf8-memory-arguments-slope", 2)
  , ("equalsByteString-cpu-arguments-constant", 245_000)
  , ("equalsByteString-cpu-arguments-intercept", 216_773)
  , ("equalsByteString-cpu-arguments-slope", 62)
  , ("equalsByteString-memory-arguments", 1)
  , ("equalsData-cpu-arguments-intercept", 1_060_367)
  , ("equalsData-cpu-arguments-slope", 12_586)
  , ("equalsData-memory-arguments", 1)
  , ("equalsInteger-cpu-arguments-intercept", 208_512)
  , ("equalsInteger-cpu-arguments-slope", 421)
  , ("equalsInteger-memory-arguments", 1)
  , ("equalsString-cpu-arguments-constant", 187_000)
  , ("equalsString-cpu-arguments-intercept", 1_000)
  , ("equalsString-cpu-arguments-slope", 52_998)
  , ("equalsString-memory-arguments", 1)
  , ("fstPair-cpu-arguments", 80_436)
  , ("fstPair-memory-arguments", 32)
  , ("headList-cpu-arguments", 43_249)
  , ("headList-memory-arguments", 32)
  , ("iData-cpu-arguments", 1_000)
  , ("iData-memory-arguments", 32)
  , ("ifThenElse-cpu-arguments", 80_556)
  , ("ifThenElse-memory-arguments", 1)
  , ("indexByteString-cpu-arguments", 57_667)
  , ("indexByteString-memory-arguments", 4)
  , ("lengthOfByteString-cpu-arguments", 1_000)
  , ("lengthOfByteString-memory-arguments", 10)
  , ("lessThanByteString-cpu-arguments-intercept", 197_145)
  , ("lessThanByteString-cpu-arguments-slope", 156)
  , ("lessThanByteString-memory-arguments", 1)
  , ("lessThanEqualsByteString-cpu-arguments-intercept", 197_145)
  , ("lessThanEqualsByteString-cpu-arguments-slope", 156)
  , ("lessThanEqualsByteString-memory-arguments", 1)
  , ("lessThanEqualsInteger-cpu-arguments-intercept", 204_924)
  , ("lessThanEqualsInteger-cpu-arguments-slope", 473)
  , ("lessThanEqualsInteger-memory-arguments", 1)
  , ("lessThanInteger-cpu-arguments-intercept", 208_896)
  , ("lessThanInteger-cpu-arguments-slope", 511)
  , ("lessThanInteger-memory-arguments", 1)
  , ("listData-cpu-arguments", 52_467)
  , ("listData-memory-arguments", 32)
  , ("mapData-cpu-arguments", 64_832)
  , ("mapData-memory-arguments", 32)
  , ("mkCons-cpu-arguments", 65_493)
  , ("mkCons-memory-arguments", 32)
  , ("mkNilData-cpu-arguments", 22_558)
  , ("mkNilData-memory-arguments", 32)
  , ("mkNilPairData-cpu-arguments", 16_563)
  , ("mkNilPairData-memory-arguments", 32)
  , ("mkPairData-cpu-arguments", 76_511)
  , ("mkPairData-memory-arguments", 32)
  , ("modInteger-cpu-arguments-constant", 196_500)
  , ("modInteger-cpu-arguments-model-arguments-intercept", 453_240)
  , ("modInteger-cpu-arguments-model-arguments-slope", 220)
  , ("modInteger-memory-arguments-intercept", 0)
  , ("modInteger-memory-arguments-minimum", 1)
  , ("modInteger-memory-arguments-slope", 1)
  , ("multiplyInteger-cpu-arguments-intercept", 69_522)
  , ("multiplyInteger-cpu-arguments-slope", 11_687)
  , ("multiplyInteger-memory-arguments-intercept", 0)
  , ("multiplyInteger-memory-arguments-slope", 1)
  , ("nullList-cpu-arguments", 60_091)
  , ("nullList-memory-arguments", 32)
  , ("quotientInteger-cpu-arguments-constant", 196_500)
  , ("quotientInteger-cpu-arguments-model-arguments-intercept", 453_240)
  , ("quotientInteger-cpu-arguments-model-arguments-slope", 220)
  , ("quotientInteger-memory-arguments-intercept", 0)
  , ("quotientInteger-memory-arguments-minimum", 1)
  , ("quotientInteger-memory-arguments-slope", 1)
  , ("remainderInteger-cpu-arguments-constant", 196_500)
  , ("remainderInteger-cpu-arguments-model-arguments-intercept", 453_240)
  , ("remainderInteger-cpu-arguments-model-arguments-slope", 220)
  , ("remainderInteger-memory-arguments-intercept", 0)
  , ("remainderInteger-memory-arguments-minimum", 1)
  , ("remainderInteger-memory-arguments-slope", 1)
  , ("serialiseData-cpu-arguments-intercept", 1_159_724)
  , ("serialiseData-cpu-arguments-slope", 392_670)
  , ("serialiseData-memory-arguments-intercept", 0)
  , ("serialiseData-memory-arguments-slope", 2)
  , ("sha2_256-cpu-arguments-intercept", 806_990)
  , ("sha2_256-cpu-arguments-slope", 30_482)
  , ("sha2_256-memory-arguments", 4)
  , ("sha3_256-cpu-arguments-intercept", 1_927_926)
  , ("sha3_256-cpu-arguments-slope", 82_523)
  , ("sha3_256-memory-arguments", 4)
  , ("sliceByteString-cpu-arguments-intercept", 265_318)
  , ("sliceByteString-cpu-arguments-slope", 0)
  , ("sliceByteString-memory-arguments-intercept", 4)
  , ("sliceByteString-memory-arguments-slope", 0)
  , ("sndPair-cpu-arguments", 85_931)
  , ("sndPair-memory-arguments", 32)
  , ("subtractInteger-cpu-arguments-intercept", 205_665)
  , ("subtractInteger-cpu-arguments-slope", 812)
  , ("subtractInteger-memory-arguments-intercept", 1)
  , ("subtractInteger-memory-arguments-slope", 1)
  , ("tailList-cpu-arguments", 41_182)
  , ("tailList-memory-arguments", 32)
  , ("trace-cpu-arguments", 212_342)
  , ("trace-memory-arguments", 32)
  , ("unBData-cpu-arguments", 31_220)
  , ("unBData-memory-arguments", 32)
  , ("unConstrData-cpu-arguments", 32_696)
  , ("unConstrData-memory-arguments", 32)
  , ("unIData-cpu-arguments", 43_357)
  , ("unIData-memory-arguments", 32)
  , ("unListData-cpu-arguments", 32_247)
  , ("unListData-memory-arguments", 32)
  , ("unMapData-cpu-arguments", 38_314)
  , ("unMapData-memory-arguments", 32)
  , ("verifyEcdsaSecp256k1Signature-cpu-arguments", 35_892_428)
  , ("verifyEcdsaSecp256k1Signature-memory-arguments", 10)
  , ("verifyEd25519Signature-cpu-arguments-intercept", 57_996_947)
  , ("verifyEd25519Signature-cpu-arguments-slope", 18_975)
  , ("verifyEd25519Signature-memory-arguments", 10)
  , ("verifySchnorrSecp256k1Signature-cpu-arguments-intercept", 38_887_044)
  , ("verifySchnorrSecp256k1Signature-cpu-arguments-slope", 32_947)
  , ("verifySchnorrSecp256k1Signature-memory-arguments", 10)
  ]
