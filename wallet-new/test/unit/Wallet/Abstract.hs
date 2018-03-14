{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Abstract definition of a wallet
module Wallet.Abstract (
    -- * Abstract definition of a wallet
    IsWallet(..)
  , Ours
  , Pending
  , walletBoot
    -- * Abstract definition of rollback
  , Rollback(..)
    -- * Inductive wallet definition
  , Inductive(..)
  , interpret
    -- ** Invariants
  , Invariant
  , invariant
    -- ** Testing
  , walletInvariants
  , walletEquivalent
    -- ** Generation
    -- $generation
  , genFromBlockchain
  , genFromBlockchainPickingAccounts
    -- * Auxiliary operations
  , balance
  , txIns
  , txOuts
  , updateUtxo
  , updatePending
  , utxoRestrictToOurs
  ) where

import qualified Data.Foldable as Fold
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Universum

import qualified Data.IntMap as IntMap
import qualified Data.List as List
import qualified Data.Text.Buildable
import           Formatting (bprint, build, sformat, (%))
import           Pos.Util (HasLens', lensOf')
import           Pos.Util.Chrono
import           Pos.Util.QuickCheck.Arbitrary (sublistN)
import           Serokell.Util (listJson)
import           Test.QuickCheck

import           Util
import           Util.Validated
import           UTxO.BlockGen (divvyUp, selectDestinations')
import           UTxO.Context
import           UTxO.Crypto
import           UTxO.DSL
import           UTxO.PreChain

{-------------------------------------------------------------------------------
  Wallet type class
-------------------------------------------------------------------------------}

-- | Check if an address is ours
type Ours a = a -> Maybe SomeKeyPair

-- | Pending transactions
type Pending h a = Set (Transaction h a)

-- | Abstract definition of a wallet
class (Hash h a, Ord a) => IsWallet w h a where
  utxo       :: w h a -> Utxo h a
  ours       :: w h a -> Ours a
  applyBlock :: Block h a -> w h a -> w h a

  -- Operations with default implementations

  pending :: w h a -> Pending h a
  default pending :: HasLens' (w h a) (Pending h a)
                  => w h a -> Pending h a
  pending w = w ^. lensOf'

  newPending :: Transaction h a -> w h a -> Maybe (w h a)
  default newPending :: HasLens' (w h a) (Pending h a)
                     => Transaction h a -> w h a -> Maybe (w h a)
  newPending tx w = do
      guard $ trIns tx `Set.isSubsetOf` utxoDomain (available w)
      return $ w & lensOf' %~ Set.insert tx

  availableBalance :: IsWallet w h a => w h a -> Value
  availableBalance = balance . available

  totalBalance :: IsWallet w h a => w h a -> Value
  totalBalance = balance . total

  available :: IsWallet w h a => w h a -> Utxo h a
  available w = utxoRemoveInputs (txIns (pending w)) (utxo w)

  change :: IsWallet w h a => w h a -> Utxo h a
  change w = utxoRestrictToOurs (ours w) (txOuts (pending w))

  total :: IsWallet w h a => w h a -> Utxo h a
  total w = available w `utxoUnion` change w

-- | Wallet state after the bootstrap transaction
walletBoot :: (IsWallet w h a, Hash h a, Ord a)
           => (Ours a -> w h a) -- ^ Wallet constructor
           -> Ours a -> Transaction h a -> w h a
walletBoot mkWallet p boot = applyBlock (OldestFirst [boot]) $ mkWallet p

{-------------------------------------------------------------------------------
  Rollback
-------------------------------------------------------------------------------}

class IsWallet w h a => Rollback w h a where
  rollback :: w h a -> w h a

{-------------------------------------------------------------------------------
  Interlude: "functor" over different wallet types (internal use only)
-------------------------------------------------------------------------------}

data Wallets :: [(* -> *) -> * -> *] -> (* -> *) -> * -> * where
  One :: IsWallet w h a
      => w h a -> Wallets '[w] h a

  Two :: (IsWallet w h a, IsWallet w' h a)
      => w h a -> w' h a -> Wallets '[w,w'] h a

walletsMap :: (forall w. IsWallet w h a => w h a -> w h a)
           -> Wallets ws h a -> Wallets ws h a
walletsMap f (One w)    = One (f w)
walletsMap f (Two w w') = Two (f w) (f w')

walletsMapA :: Applicative f
            => (forall w. IsWallet w h a => w h a -> f (w h a))
            -> Wallets ws h a -> f (Wallets ws h a)
walletsMapA f (One w)    = One <$> f w
walletsMapA f (Two w w') = Two <$> f w <*> f w'

{-------------------------------------------------------------------------------
  Inductive wallet definition
-------------------------------------------------------------------------------}

-- | Inductive definition of a wallet
data Inductive h a =
    -- | Start the wallet, given the bootstrap transaction
    WalletBoot (Transaction h a)

    -- | Inform the wallet of a new block added to the blockchain
  | ApplyBlock (Inductive h a) (Block h a)

    -- | Submit a new transaction to the wallet to be included in the blockchain
  | NewPending (Inductive h a) (Transaction h a)
  deriving Eq

-- | Interpreter for 'Inductive'
--
-- Given (one or more) empty wallets, evaluate an 'Inductive' wallet, checking
-- the given property at each step.
--
-- Note: we expect the 'Inductive' to be valid (valid blockchain, valid
-- calls to 'newPending', etc.). This is meant to check properties of the
-- /wallet/, not the wallet input.
interpret :: forall ws h a.
             (Buildable a)
          => (Transaction h a -> Wallets ws h a)    -- ^ Bootstrapped wallets
          -> (Wallets ws h a -> Validated Text ())  -- ^ Predicate to check
          -> Inductive h a -> Validated Text (Wallets ws h a)
interpret mkWallets p ind = fmap snd $ go ind
  where
    -- Evaluate and verify the 'Inductive'
    -- Also returns the ledger after validation
    go :: Inductive h a -> Validated Text (Ledger h a, Wallets ws h a)
    go (WalletBoot t)   = do
      ws <- verify (mkWallets t)
      return (ledgerSingleton t, ws)
    go (ApplyBlock w b) = do
      (l, ws) <- go w
      ws' <- verify $ walletsMap (applyBlock b) ws
      return (ledgerAdds (toNewestFirst b) l, ws')
    go (NewPending w t) = do
      (l, ws) <- go w
      ws' <- verify =<< walletsMapA (verifyNew l t) ws
      return (l, ws')

    verify :: Wallets ws h a -> Validated Text (Wallets ws h a)
    verify ws = p ws >> return ws

    verifyNew :: IsWallet w h a
              => Ledger h a    -- ^ Ledger so far (for error messages)
              -> Transaction h a -> w h a -> Validated Text (w h a)
    verifyNew l tx w =
        case newPending tx w of
          Just w' -> return w'
          Nothing -> throwError $ pretty (InvalidPending tx (utxo w) (pending w) l ind)

-- | The 'Inductive' contains an invalid pending transaction
--
-- This indicates a bug in the generator (or in the hand-written 'Inductive'),
-- so we try to provide sufficient information to track that down.
data InvalidPending h a = InvalidPending {
      -- | The submitted transaction that was invalid
      invalidPendingTransaction :: Transaction h a

      -- | The UTxO of the wallet at the time of submission
    , invalidPendingWalletUtxo :: Utxo h a

      -- | The pending set of the wallet at time of submission
    , invalidPendingWalletPending :: Pending h a

      -- | The ledger seen so far at the time of submission
    , invalidPendingLedger :: Ledger h a

      -- | The /entire/ 'Inductive' wallet that we are processing
    , invalidPendingInductive :: Inductive h a
    }

{-------------------------------------------------------------------------------
  Invariants
-------------------------------------------------------------------------------}

-- | Wallet invariant
--
-- A wallet invariant is a property that is preserved by the fundamental
-- wallet operations, as defined by the 'IsWallet' type class and the
-- definition of 'Inductive'.
--
-- In order to evaluate the inductive definition we need the empty wallet
-- to be passed as a starting point.
type Invariant h a = Inductive h a -> Validated Text ()

-- | Lift a property of flat wallet values to an invariant over the wallet ops
invariant :: (IsWallet w h a, Buildable a)
          => Text                        -- ^ Error msg if property violated
          -> (Transaction h a -> w h a)  -- ^ Construct empty wallet
          -> (w h a -> Bool)             -- ^ Property to verify
          -> Invariant h a
invariant err e p = void . interpret (One . e) p'
  where
    p' (One w) = unless (p w) $ throwError err

{-------------------------------------------------------------------------------
  Specific invariants
-------------------------------------------------------------------------------}

-- | Wallet invariant, parameterized by a function to construct the wallet
type WalletInv w h a = (IsWallet w h a, Buildable a)
                    => (Transaction h a -> w h a) -> Invariant h a

walletInvariants :: WalletInv w h a
walletInvariants e w = sequence_ [
      pendingInUtxo          e w
    , utxoIsOurs             e w
    , changeNotAvailable     e w
    , changeNotInUtxo        e w
    , changeAvailable        e w
    , balanceChangeAvailable e w
    ]

pendingInUtxo :: WalletInv w h a
pendingInUtxo e = invariant "pendingInUtxo" e $ \w ->
    txIns (pending w) `Set.isSubsetOf` utxoDomain (utxo w)

utxoIsOurs :: WalletInv w h a
utxoIsOurs e = invariant "utxoIsOurs" e $ \w ->
    all (isJust . ours w . outAddr) (utxoRange (utxo w))

changeNotAvailable :: WalletInv w h a
changeNotAvailable e = invariant "changeNotAvailable" e $ \w ->
    utxoDomain (change w) `disjoint` utxoDomain (available w)

changeNotInUtxo :: WalletInv w h a
changeNotInUtxo e = invariant "changeNotInUtxo" e $ \w ->
    utxoDomain (change w) `disjoint` utxoDomain (utxo w)

changeAvailable :: WalletInv w h a
changeAvailable e = invariant "changeAvailable" e $ \w ->
    change w `utxoUnion` available w == total w

balanceChangeAvailable :: WalletInv w h a
balanceChangeAvailable e = invariant "balanceChangeAvailable" e $ \w ->
    balance (change w) + balance (available w) == balance (total w)

{-------------------------------------------------------------------------------
  Compare different wallet implementations
-------------------------------------------------------------------------------}

walletEquivalent :: forall w w' h a.
                    ( IsWallet w  h a
                    , IsWallet w' h a
                    , Buildable a
                    )
                 => (Transaction h a -> w  h a)
                 -> (Transaction h a -> w' h a)
                 -> Invariant h a
walletEquivalent e e' =
    void . interpret (\boot -> Two (e boot) (e' boot)) p
  where
    p :: Wallets '[w,w'] h a -> Validated Text ()
    p (Two w w') = sequence_ [
          cmp "pending"          pending
        , cmp "utxo"             utxo
        , cmp "availableBalance" availableBalance
        , cmp "totalBalance"     totalBalance
        , cmp "available"        available
        , cmp "change"           change
        , cmp "total"            total
        ]
      where
        cmp :: Eq b
            => Text
            -> (forall w''. IsWallet w'' h a => w'' h a -> b)
            -> Validated Text ()
        cmp err f = unless (f w == f w') $ throwError err

{-------------------------------------------------------------------------------
  Auxiliary operations
-------------------------------------------------------------------------------}

balance :: Utxo h a -> Value
balance = sum . map outVal . Map.elems . utxoToMap

txIns :: (Hash h a, Foldable f) => f (Transaction h a) -> Set (Input h a)
txIns = Set.unions . map trIns . Fold.toList

txOuts :: (Hash h a, Foldable f) => f (Transaction h a) -> Utxo h a
txOuts = utxoUnions . map trUtxo . Fold.toList

updateUtxo :: forall h a. Hash h a
           => Ours a -> Block h a -> Utxo h a -> Utxo h a
updateUtxo p b = remSpent . addNew
  where
    addNew, remSpent :: Utxo h a -> Utxo h a
    addNew   = utxoUnion (utxoRestrictToOurs p (txOuts b))
    remSpent = utxoRemoveInputs (txIns b)

updatePending :: forall h a. Hash h a => Block h a -> Pending h a -> Pending h a
updatePending b = Set.filter $ \t -> disjoint (trIns t) (txIns b)

utxoRestrictToOurs :: Ours a -> Utxo h a -> Utxo h a
utxoRestrictToOurs p = utxoRestrictToAddr (isJust . p)

{-------------------------------------------------------------------------------
  Generation
-------------------------------------------------------------------------------}

-- $generation
--
-- The 'Inductive' data type describes a potential history of a wallet's
-- view of an existing blockchain. This means that there are many possible
-- 'Inductive's for any given blockchain -- any set of addresses can belong
-- to the 'Inductive' that the wallet is for, and there are many possible
-- sequences of actions that adequately describe the view of the
-- blockchain.

-- | A monad for generating inductive chains.
newtype InductiveGen h a
    = InductiveGen
    { unInductiveGen :: ReaderT (InductiveCtx h) Gen a
    } deriving (Functor, Applicative, Monad, MonadReader (InductiveCtx h))

runInductiveGen :: FromPreChain h () -> InductiveGen h a -> Gen a
runInductiveGen fpc ig = runReaderT (unInductiveGen ig) (initializeCtx fpc)

data InductiveCtx h
    = InductiveCtx
    { icFromPreChain  :: !(FromPreChain h ())
    }

initializeCtx :: FromPreChain h () -> InductiveCtx h
initializeCtx fpc = InductiveCtx{..}
  where
    icFromPreChain = fpc

getFromPreChain :: InductiveGen h (FromPreChain h ())
getFromPreChain = asks icFromPreChain

getBootstrap :: InductiveGen h (Transaction h Addr)
getBootstrap = fpcBoot <$> getFromPreChain

getBlockchain :: InductiveGen h (Chain h Addr)
getBlockchain = fpcChain <$> getFromPreChain

getLedger :: InductiveGen h (Ledger h Addr)
getLedger = fpcLedger <$> getFromPreChain

getBootTransaction :: InductiveGen h (Transaction h Addr)
getBootTransaction = fpcBoot <$> getFromPreChain

-- | The 'Inductive' data type is isomorphic to a linked list of this
-- 'Action' type. It is more convenient to operate on this type, as it can
-- vary the sequence representation and reuse sequence functions.
data Action h a
    = ApplyBlock' (Block h a)
    | NewPending' (Transaction h a)

-- | Smart constructor that adds the callstack to the transaction's comments
-- (Useful for finding out where transactions are coming from)
newPending' :: HasCallStack => [Text] -> Transaction h a -> Action h a
newPending' extra t = NewPending' (t { trExtra = trExtra t ++ extra })

-- | Convert a container of 'Action's into an 'Inductive' wallet,
-- given the bootstrap transaction.
toInductive :: (Hash h a, Buildable a) => Transaction h a -> [Action h a] -> Inductive h a
toInductive boot = foldl' k (WalletBoot boot)
  where
    k acc (ApplyBlock' a) = ApplyBlock acc a
    k acc (NewPending' a) = NewPending acc a

-- | Given a 'Set' of addresses that will represent the addresses that
-- belong to the generated 'Inductive' wallet and the 'FromPreChain' value
-- that contains the relevant blockchain, this will be able to generate
-- arbitrary views into the blockchain.
genFromBlockchain
    :: Hash h Addr
    => Set Addr
    -> FromPreChain h ()
    -> Gen (Inductive h Addr)
genFromBlockchain addrs fpc =
    runInductiveGen fpc (genInductiveFor addrs)

-- | Selects a random subset of addresses to be considered from the
-- blockchain in the amount given.
genFromBlockchainPickingAccounts
    :: Hash h Addr
    => Int
    -> FromPreChain h ()
    -> Gen (Set Addr, Inductive h Addr)
genFromBlockchainPickingAccounts i fpc = do
    let allAddrs = toList (ledgerAddresses (fpcLedger fpc))
        eligibleAddrs = filter (not . isAvvmAddr) allAddrs

    if null eligibleAddrs then
        error
        $ sformat
            ( "No eligible addresses!\n\n"
            % "All addresses: " % build
            ) (intercalate ", " (map show allAddrs))
        else pure ()

    addrs <- Set.fromList <$> sublistN i eligibleAddrs

    if null addrs then
        error
        $ sformat
            ( "No addresses!\n\n"
            % "All addresses: " % build
            ) (intercalate ", " (map show allAddrs))
        else pure ()

    (,) addrs <$> genFromBlockchain addrs fpc

genInductiveFor :: Hash h Addr => Set Addr -> InductiveGen h (Inductive h Addr)
genInductiveFor addrs = do
    boot  <- getBootstrap
    chain <- getBlockchain
    intersperseTransactions boot addrs (chainToApplyBlocks chain)

-- | The first step in converting a 'Chain into an 'Inductive' wallet is
-- to sequence the existing blocks using 'ApplyBlock' constructors.
chainToApplyBlocks :: Chain h a -> [Action h a]
chainToApplyBlocks = toList . map ApplyBlock' . chainBlocks

-- | Once we've created our initial @['Action' h 'Addr']@, we want to
-- insert some 'Transaction's in appropriate locations in the list. There
-- are some properties that the inserted events must satisfy:
--
-- * The transaction must be after all of the blocks that confirm inputs to
--   the transaction.
-- * The transaction must be before the block that confirms it, if any
--   blocks confirm it. It is not necessary that the transaction gets
--   confirmed eventually!
--
-- See Note [Intersperse]
intersperseTransactions
    :: Hash h Addr
    => Transaction h Addr -- ^ Bootstrap transaction
    -> Set Addr           -- ^ " Our " addresses
    -> [Action h Addr]    -- ^ Initial actions (the blocks in the chain)
    -> InductiveGen h (Inductive h Addr)
intersperseTransactions boot addrs actions = do
    chain <- getBlockchain
    ledger <- getLedger
    let ourTxns = findOurTransactions addrs ledger chain
    let allTxnCount = length ourTxns

    -- we weight the frequency distribution such that most of the
    -- transactions will be represented by this wallet. this can be
    -- changed or made configurable later.
    --
    -- also, weirdly, sometimes there aren't any transactions on any of the
    -- addresses that belong to us. that seems like an edge case.
    txnToDisperseCount <- if allTxnCount == 0
        then pure 0
        else liftGen
            . frequency
            . zip [1 .. allTxnCount]
            . map pure
            $ [1 .. allTxnCount]

    txnsToDisperse <- liftGen $ sublistN txnToDisperseCount ourTxns


    let txnsWithRange =
            mapMaybe
                (\(i, t) -> (,,) t i <$> transactionFullyConfirmedAt addrs t chain ledger)
                txnsToDisperse

    let chooseBlock t lo hi i =
          (t { trExtra = sformat ("Inserted at "
                                 % build
                                 % " <= "
                                 % build
                                 % " < "
                                 % build
                                 ) lo i hi : trExtra t }, i)
    txnsWithIndex <- fmap catMaybes $
        forM txnsWithRange $ \(t, hi, lo) ->
          if hi > lo then
            Just . chooseBlock t lo hi <$> liftGen (choose (lo, hi - 1))
          else
            -- cannot create a pending transaction from a transaction that uses
            -- inputs from the very same block in which it gets confirmed
            return Nothing

    let append    = flip (<>)
        spent     = Set.unions $ map (trIns . fst) txnsWithIndex
        confirmed =
            foldr
                (\(t, i) -> IntMap.insertWith append i [newPending' [] t])
                (dissect actions)
                txnsWithIndex

    unconfirmed <- synthesizeTransactions addrs spent

    return $ toInductive boot
           . conssect
           $ IntMap.unionWith (<>) confirmed unconfirmed

-- | Generate transactions that will never be confirmed
--
-- We take as argument the set of inputs already spent by transactions that
-- /will/ be confirmed, so that we don't create an 'Inductive' wallet with
-- double spending.
synthesizeTransactions
    :: forall h. Hash h Addr
    => Set Addr           -- ^ Addresses owned by the wallet
    -> Set (Input h Addr) -- ^ Inputs already spent
    -> InductiveGen h (IntMap [Action h Addr])
synthesizeTransactions addrs alreadySpent = do
    boot   <- getBootTransaction
    blocks <- toList . chainBlocks <$> getBlockchain
    liftGen $ go IntMap.empty (trUtxo boot) alreadySpent 0 blocks
  where
    -- NOTE: We maintain a UTxO as we process the blocks. There are (at least)
    -- two alternatives here:
    --
    -- * Depend on a wallet implementation to keep track both of the utxo and
    --   of the set of pending transactions. That would make the tests
    --   kind of circular though (using the wallet to test the wallet);
    --   better to keep the two independent.
    -- * We could reuse the UTxOs that we compute while we compute the chain.
    --   This is certainly a possibility, /but/ there is a downside there:
    --   those UTxOs are approximate only, as we don't have any fees.
    --   The chain as we have it here has proper fee values.
    --   (Having said that, we still don't have proper fee values for the
    --   transactions we generate here.)
    go :: IntMap [Action h Addr]  -- Accumulator
       -> Utxo h Addr             -- Current utxo
       -> Set (Input h Addr)      -- All inputs already spent
       -> Int                     -- Index of the next block
       -> [Block h Addr]          -- Chain yet to process
       -> Gen (IntMap [Action h Addr])
    go acc _ _ _ [] =
        -- We could create some pending transactions after the very last block,
        -- but we don't
        return acc
    go acc utxoBefore spent ix (b:bs) = do
        pct <- choose (0, 100 :: Int)
        if pct >= 5 || utxoNull utxoAvail
          then go acc utxoAfter spent (ix + 1) bs
          else do
            (i, o) <- elements $ utxoToList utxoAvail
            dests  <- selectDestinations' Set.empty utxoAfter
            let txn = divvyUp newHash (pure (i, o)) dests 0
                act = newPending' ["never confirmed"] txn
            go (IntMap.insert ix [act] acc)
               utxoAfter
               (Set.insert i spent)
               (ix + 1)
               bs
      where
        utxoAfter = utxoApplyBlock b utxoBefore
        utxoAvail = oursNotSpent spent utxoAfter
        newHash   = negate ix -- negative hashes not used elsewhere

    oursNotSpent :: Set (Input h Addr) -> Utxo h Addr -> Utxo h Addr
    oursNotSpent spent = utxoRemoveInputs spent
                       . utxoRestrictToAddr (`Set.member` addrs)

-- | Construct an 'IntMap' consisting of the index of the element in the
-- input list pointing to a singleton list of the element the original
-- list.
dissect :: [a] -> IntMap [a]
dissect = IntMap.fromList . zip [0..] . map pure

-- | Reverse the operation of 'dissect'. Given an 'IntMap' originally
-- representing the original index in the list pointing to the list of new
-- items at that starting index, collapse that into a single list of
-- elements.
conssect :: IntMap [a] -> [a]
conssect = concatMap snd . IntMap.toList

-- | Given a 'Set' of addresses and a 'Chain', this function returns a list
-- of the transactions with *inputs* belonging to any of the addresses and
-- the index of the block that the transaction is confirmed in.
findOurTransactions
    :: (Hash h a, Ord a)
    => Set a
    -> Ledger h a
    -> Chain h a
    -> [(Int, Transaction h a)]
findOurTransactions addrs ledger =
    concatMap k . zip [0..] . toList . chainBlocks
  where
    k (i, block) =
        map ((,) i)
            . filter (all p . trIns)
            $ toList block
    p = fromMaybe False
        . fmap (\o -> outAddr o `Set.member` addrs)
        . (`inpSpentOutput` ledger)

-- | This function identifies the index of the block that the input was
-- received in the ledger, marking the point at which it may be inserted as
-- a 'NewPending' transaction.
blockReceivedIndex :: Hash h Addr => Input h Addr -> Chain h Addr -> Maybe Int
blockReceivedIndex i =
    List.findIndex (any ((inpTrans i ==) . hash)) . toList . chainBlocks

-- | For each 'Input' in the 'Transaction' that belongs to one of the
-- 'Addr'esses in the 'Set' provided, find the index of the block in the
-- 'Chain' that confirms that 'Input'. Take the maximum index and return
-- that -- that is the earliest this transaction may appear as a pending
-- transaction.
transactionFullyConfirmedAt
    :: Hash h Addr
    => Set Addr
    -> Transaction h Addr
    -> Chain h Addr
    -> Ledger h Addr
    -> Maybe Int
transactionFullyConfirmedAt addrs txn chain ledger =
    let inps = Set.filter inputInAddrs (trIns txn)
        inputInAddrs i =
            case inpSpentOutput i ledger of
                Just o  -> outAddr o `Set.member` addrs
                Nothing -> False
        indexes = Set.map (\i -> blockReceivedIndex i chain) inps
     in foldl' max Nothing indexes

liftGen :: Gen a -> InductiveGen h a
liftGen = InductiveGen . lift

{- Note [Intersperse]
~~~~~~~~~~~~~~~~~~~~~
Given a list of blocks

> [ applyBlock_0, applyBlock_1, applyBlock_2, applyBlock_3, applyBlock_4 ]

we construct an 'IntMap' out of them where the index in the intmap is the
original index of that block in the chain:

> { 0 -> [applyBlock_0]
> , 1 -> [applyBlock_1]
> , 2 -> [applyBlock_2]
> , 3 -> [applyBlock_3]
> , 4 -> [applyBlock_4]
> }

Then, when we select an index between @lo@ (the latest block to confirm an input
in the transaction) and @hi@ (the index of the block that confirms the
transaction itself), we can 'insertWith' at that index. Suppose we have a
transaction @t@ where the input was provided in block 1 and is confirmed in
block 3. That means we can have @NewPending t@ inserted into either index 1 or
2:

> { 0 -> [applyBlock_0]
> , 1 -> [applyBlock_1] <> [newPending t] = [applyBlock_1, newPending t]
> , 2 -> [applyBlock_2]
> , 3 -> [applyBlock_3]
> , 4 -> [applyBlock_4]
> }

or

> { 0 -> [applyBlock_0]
> , 1 -> [applyBlock_1]
> , 2 -> [applyBlock_2] <> [newPending t] = [applyBlock_2, newPending t]
> , 3 -> [applyBlock_3]
> , 4 -> [applyBlock_4]
> }

Then, when we finally go to 'conssec' the @IntMap [Action h a]@ back into a
@[Action h a]@, we get:

> [ applyBlock_0
> , applyBlock_1
> , applyBlock_2, newPending t
> , applyBlock_3
> , applyBlock_4
> ]

TODO: This means that currently we never insert pending transactions before
the first block.
-}

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance (Hash h a, Buildable a) => Buildable (InvalidPending h a) where
  build InvalidPending{..} = bprint
    ( "InvalidPending "
    % "{ transaction:   " % build
    % ", walletUtxo:    " % build
    % ", walletPending: " % listJson
    % ", ledger:        " % build
    % ", inductive:     " % build
    % "}"
    )
    invalidPendingTransaction
    invalidPendingWalletUtxo
    invalidPendingWalletPending
    invalidPendingLedger
    invalidPendingInductive

-- | We output the inductive in the order that things are applied; something like
--
-- > { "boot": <boot transaction>
-- > , "block": <first block>
-- > , "new": <first transaction>
-- > , "block": <second block>
-- > ..
-- > }
instance (Hash h a, Buildable a) => Buildable (Inductive h a) where
  build ind = bprint (build % "}") (go ind)
    where
      go (WalletBoot   t) = bprint (        "{ boot:  " % build)        t
      go (ApplyBlock n b) = bprint (build % ", block: " % build) (go n) b
      go (NewPending n t) = bprint (build % ", new:   " % build) (go n) t

instance (Hash h a, Buildable a) => Buildable (Action h a) where
  build (ApplyBlock' b) = bprint ("ApplyBlock' " % build) b
  build (NewPending' t) = bprint ("NewPending' " % build) t

instance (Hash h a, Buildable a) => Buildable [Action h a] where
  build = bprint listJson
