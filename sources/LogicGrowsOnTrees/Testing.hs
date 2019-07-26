{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module LogicGrowsOnTrees.Testing where

import Control.Applicative
import Control.Arrow (second)
import Control.Lens (_1,_2,(%=),(<+=),use)
import Control.Monad
import Control.Monad.Catch (Exception)
import Control.Monad.Operational (ProgramViewT(..),view)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.State (StateT,evalStateT)

import Data.Bits
import Data.Functor.Identity
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import Data.IORef
import Data.Maybe
import Data.Monoid
import Data.Sequence ((<|))
import qualified Data.Sequence as Seq
import qualified Data.Serialize as Serialize
import Data.Serialize (Serialize(),encode)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Typeable
import qualified Data.UUID as UUID
import Data.UUID (UUID)

import Text.Printf

import Test.Framework (Test,testGroup)
import Test.HUnit (Assertion,assertBool,assertEqual)
import Test.QuickCheck (Testable)
import Test.QuickCheck.Arbitrary hiding ((><))
import Test.QuickCheck.Gen hiding (shuffle)
import Test.QuickCheck.Instances ()
import Test.SmallCheck.Series (Serial(..))

import LogicGrowsOnTrees
import LogicGrowsOnTrees.Checkpoint
import LogicGrowsOnTrees.Location
import LogicGrowsOnTrees.Parallel.ExplorationMode
import LogicGrowsOnTrees.Path
import LogicGrowsOnTrees.Parallel.Common.Supervisor
import LogicGrowsOnTrees.Workload
import LogicGrowsOnTrees.Parallel.Common.Worker
-- }}}

-- Helpers {{{

-- Instances {{{
-- Newtypes {{{
newtype UniqueTreeT m = UniqueTree { unwrapUniqueTree :: TreeT m IntSet }
-- }}}

-- Arbitrary {{{
instance Arbitrary BranchChoice where arbitrary = elements [LeftBranch,RightBranch]

instance (Arbitrary α, Monoid α, Serialize α, Functor m, Monad m) ⇒ Arbitrary (TreeT m α) where -- {{{
    arbitrary = fmap ($ mempty) (sized arb)
      where
        arb :: Monoid α ⇒ Int → Gen (α → TreeT m α)
        arb 0 = nullPlus
        arb 1 = frequency
            [(1,nullPlus)
            ,(1,processPlus)
            ,(2,resultPlus)
            ,(2,cachedPlus)
            ]
        arb n = frequency
            [(1,liftM2 (>=>) processPlus (arb n))
            ,(2,liftM2 (>=>) resultPlus (arb n))
            ,(2,liftM2 (>=>) cachedPlus (arb n))
            ,(4, do left_size ← choose (0,n)
                    let right_size = n-left_size
                    liftM2 (liftA2 mplus)
                        (arb left_size)
                        (arb right_size)
             )
            ]

        nullPlus :: Gen (α → TreeT m α)
        nullPlus = return (const mzero)

        result, cached :: Gen (TreeT m α)
        result = fmap return arbitrary
        cached = fmap cache arbitrary

        resultPlus, cachedPlus :: Monoid α ⇒ Gen (α → TreeT m α)
        resultPlus = (\x → flip fmap x . mappend) <$> result
        cachedPlus = (\x → flip fmap x . mappend) <$> cached

        processPlus :: Gen (α → TreeT m α)
        processPlus = return processPendingRequestsAndReturn
-- }}}
-- }}}

instance Monad m ⇒ Arbitrary (UniqueTreeT m) where -- {{{
    arbitrary = (UniqueTree . ($ (const $ return ()))) <$> randomUniqueTreeWithHooks
-- }}}

instance Arbitrary Checkpoint where -- {{{
    arbitrary = sized arb
      where
        arb 0 = elements [Explored,Unexplored]
        arb n = frequency
                    [(1,return Explored)
                    ,(1,return Unexplored)
                    ,(1,liftM2 CachePoint (fmap encode (arbitrary :: Gen Int)) (arb (n-1)))
                    ,(2,liftM2 ChoicePoint (arb (n `div` 2)) (arb (n `div` 2)))
                    ]
-- }}}

instance Arbitrary Location where arbitrary = fmap labelFromBranching (arbitrary :: Gen [BranchChoice])

instance Arbitrary Step where -- {{{
    arbitrary = oneof
        [CacheStep <$> arbitrary
        ,ChoiceStep <$> arbitrary
        ]
-- }}}

instance Arbitrary α ⇒ Arbitrary (Solution α) where -- {{{
    arbitrary = Solution <$> arbitrary <*> arbitrary
-- }}}

instance Arbitrary α ⇒ Arbitrary (Progress α) where -- {{{
    arbitrary = Progress <$> arbitrary <*> arbitrary
-- }}}

instance Arbitrary α ⇒ Arbitrary (ProgressUpdate α) where -- {{{
    arbitrary = ProgressUpdate <$> arbitrary <*> arbitrary
-- }}}

instance Arbitrary α ⇒ Arbitrary (StolenWorkload α) where -- {{{
    arbitrary = StolenWorkload <$> arbitrary <*> arbitrary
-- }}}

instance Arbitrary Workload where -- {{{
    arbitrary = Workload <$> arbitrary <*> arbitrary
-- }}}
-- }}}

-- Serial {{{
instance Serial IO All where series = All <$> series
instance Serial IO Any where series = Any <$> series
instance Serial IO Word where series = (fromIntegral :: Int → Word) . abs <$> series
instance Serial IO (Sum Int) where series = Sum <$> series
instance Serial IO (Set String) where series = Set.fromList <$> series
-- }}}

-- Serialize {{{
instance Serialize UUID where -- {{{
    put = Serialize.putLazyByteString . UUID.toByteString
    get = fromJust . UUID.fromByteString <$> Serialize.getLazyByteString 16
-- }}}

instance Serialize (Sum Int) where -- {{{
    put = Serialize.put . getSum
    get = fmap Sum Serialize.get
-- }}}
-- }}} Serialize

-- Show {{{
instance Show UniqueTree where show = show . unwrapUniqueTree
-- }}}
-- }}}

-- Exceptions {{{
-- TestException {{{
data TestException = TestException Int deriving (Eq,Show,Typeable)
instance Exception TestException
-- }}}
-- }}}

-- Type alises {{{
type UniqueTree = UniqueTreeT Identity
-- }}}

-- Functions {{{
addAcceptOneWorkloadAction :: -- {{{
    SupervisorCallbacks exploration_mode worker_id IO →
    IO (IORef (Maybe (worker_id,Workload)),SupervisorCallbacks exploration_mode worker_id IO)
addAcceptOneWorkloadAction actions = do
    maybe_worker_and_workload_ref ← newIORef (Nothing :: Maybe (worker_id,Workload))
    return (maybe_worker_and_workload_ref, actions {
        sendWorkloadToWorker = \workload worker_id → do
            maybe_old_workload ← readIORef maybe_worker_and_workload_ref
            case maybe_old_workload of
                Nothing → return ()
                Just _ → error "workload has been submitted already!"
            writeIORef maybe_worker_and_workload_ref $ Just (worker_id,workload)
    })
-- }}}

addAcceptMultipleWorkloadsAction :: -- {{{
    SupervisorCallbacks exploration_mode worker_id IO →
    IO (IORef [(worker_id,Workload)],SupervisorCallbacks exploration_mode worker_id IO)
addAcceptMultipleWorkloadsAction actions = do
    workers_and_workloads_ref ← newIORef []
    return (workers_and_workloads_ref, actions {
        sendWorkloadToWorker = \workload worker_id →
            readIORef workers_and_workloads_ref
            >>=
            writeIORef workers_and_workloads_ref . (++ [(worker_id,workload)])
    })
-- }}}

addAppendWorkloadStealBroadcastIdsAction :: -- {{{
    SupervisorCallbacks exploration_mode worker_id IO →
    IO (IORef [[worker_id]],SupervisorCallbacks exploration_mode worker_id IO)
addAppendWorkloadStealBroadcastIdsAction actions = do
    broadcasts_ref ← newIORef ([] :: [[worker_id]])
    return (broadcasts_ref, actions {
        broadcastWorkloadStealToWorkers = \worker_ids →
            modifyIORef broadcasts_ref (++ [worker_ids])
    })
-- }}}

addAppendProgressBroadcastIdsAction :: -- {{{
    SupervisorCallbacks exploration_mode worker_id IO →
    IO (IORef [[worker_id]],SupervisorCallbacks exploration_mode worker_id IO)
addAppendProgressBroadcastIdsAction actions = do
    broadcasts_ref ← newIORef ([] :: [[worker_id]])
    return (broadcasts_ref, actions {
        broadcastProgressUpdateToWorkers = \worker_ids →
            modifyIORef broadcasts_ref (++ [worker_ids])
    })
-- }}}

addReceiveCurrentProgressAction :: -- {{{
    SupervisorCallbacks exploration_mode worker_id IO →
    IO (IORef (Maybe (ProgressFor exploration_mode)),SupervisorCallbacks exploration_mode worker_id IO)
addReceiveCurrentProgressAction actions = do
    maybe_progress_ref ← newIORef (Nothing :: Maybe ip)
    return (maybe_progress_ref, actions {
        receiveCurrentProgress = \progress → do
            maybe_old_progress ← readIORef maybe_progress_ref
            case maybe_old_progress of
                Nothing → return ()
                Just _ → error "progress update has been received already!"
            writeIORef maybe_progress_ref $ Just progress
    })
-- }}}

addSetWorkloadStealBroadcastIdsAction :: -- {{{
    SupervisorCallbacks exploration_mode worker_id IO →
    IO (IORef [worker_id],SupervisorCallbacks exploration_mode worker_id IO)
addSetWorkloadStealBroadcastIdsAction actions = do
    broadcasts_ref ← newIORef ([] :: [worker_id])
    return (broadcasts_ref, actions {
        broadcastWorkloadStealToWorkers = writeIORef broadcasts_ref
    })
-- }}}

checkFoundAgainstThreshold :: Int → IntSet → (IntSet,Bool) → IO Bool -- {{{
checkFoundAgainstThreshold threshold solutions (result,found)
  | found = do
        assertBool "check that the result set is big enough" $ IntSet.size result >= threshold
        assertBool "check that the results are all in the full set of solutions" $ result `IntSet.isSubsetOf` solutions
        return True
  | otherwise = do
        assertBool (printf "check that the unsuccessful result is small enough (%i < %i)" (IntSet.size result) threshold) $ IntSet.size result < threshold
        assertEqual "check that the result equals the solutions" solutions result
        return True
-- }}}
-- }}}
-- }}}

ignoreAcceptWorkloadAction :: -- {{{
    SupervisorCallbacks exploration_mode worker_id IO →
    SupervisorCallbacks exploration_mode worker_id IO
ignoreAcceptWorkloadAction actions = actions { sendWorkloadToWorker = \_ _ → return () }
-- }}}

ignoreWorkloadStealAction :: -- {{{
    SupervisorCallbacks exploration_mode worker_id IO →
    SupervisorCallbacks exploration_mode worker_id IO
ignoreWorkloadStealAction actions = actions { broadcastWorkloadStealToWorkers = \_ → return () }
-- }}}

frequencyT :: (MonadTrans t, Monad (t Gen)) ⇒ [(Int, t Gen a)] → t Gen a -- {{{
frequencyT [] = error "frequencyT used with empty list"
frequencyT xs0 = lift (choose (1, tot)) >>= pick xs0
 where
  tot = sum (map fst xs0)

  pick ((k,x):xs) n
    | n <= k    = x
    | otherwise = pick xs (n-k)
  pick _ _  = error "frequencyT.pick used with empty list"
-- }}}

shuffle :: [α] → Gen [α] -- {{{
shuffle [] = return []
shuffle items = do
    index ← choose (0,length items-1)
    let hd = items !! index
        rest = take index items ++ drop (index+1) items
    tl ← shuffle rest
    return (hd:tl)
-- }}}

processPendingRequestsAndReturn :: Monad m ⇒ α → TreeT m α -- {{{
processPendingRequestsAndReturn x = processPendingRequests >> return x
-- }}}

randomCheckpointForTree :: Monoid α ⇒ Tree α → Gen (α,Checkpoint) -- {{{
randomCheckpointForTree (TreeT tree) = go1 tree
  where
    go1 tree = frequency
        [(1,return (exploreTree (TreeT tree),Explored))
        ,(1,return (mempty,Unexplored))
        ,(3,go2 tree)
        ]
    go2 (view → Cache (Identity (Just x)) :>>= k) =
        fmap (second $ CachePoint (encode x)) (go1 (k x))
    go2 (view → Choice (TreeT x) (TreeT y) :>>= k) =
        liftM2 (\(left_result,left) (right_result,right) →
            (left_result `mappend` right_result, ChoicePoint left right)
        ) (go1 (x >>= k)) (go1 (y >>= k))
    go2 (view → ProcessPendingRequests :>>= k) = go2 (k ())
    go2 tree = elements [(exploreTree (TreeT tree),Explored),(mempty,Unexplored)]
-- }}}

randomNullTreeWithHooks :: ∀ m. Monad m ⇒ Gen ((Int → m ()) → TreeT m IntSet) -- {{{
randomNullTreeWithHooks = fmap (($ 0) . curry) . sized $ \n → evalStateT (arb1 n 0) (-1,IntSet.empty)
  where
    arb1, arb2 :: Int → Int → StateT (Int,IntSet) Gen ((Int,Int → m ()) → TreeT m IntSet)

    arb1 n intermediate = do
        hook_id ← _1 <+= 1
        tree ← arb2 n intermediate
        return $ \args@(_,runHook) → lift (runHook hook_id) >> tree args

    arb2 0 _ = return (const mzero)
    arb2 1 _ = return (const mzero)
    arb2 n intermediate = frequencyT
        [(1,generateForNext processPendingRequestsAndReturn intermediate (arb1 n))
        ,(2,generateForNext return intermediate (arb1 n))
        ,(2,generateForNext cache intermediate (arb1 n))
        ,(4, do left_size ← lift $ choose (0,n)
                let right_size = n-left_size
                liftM2 (liftA2 mplus)
                    (arb1 left_size intermediate)
                    (arb1 right_size intermediate)
         )
        ]

    generateForNext :: -- {{{
        Monad m ⇒
        (Int → TreeT m Int) →
        Int →
        (Int → StateT (Int,IntSet) Gen ((Int,Int → m ()) → TreeT m IntSet)) →
        StateT (Int,IntSet) Gen ((Int,Int → m ()) → TreeT m IntSet)
    generateForNext construct intermediate next = do
        x ← lift arbitrary
        let new_intermediate = x `xor` intermediate
        tree ← next new_intermediate
        return $ \(value,runHook) → do
            new_value ← construct . xor x $ value
            tree (new_value,runHook)
    -- }}}
-- }}}

randomPathForTree :: Tree α → Gen Path -- {{{
randomPathForTree (TreeT tree) = go tree
  where
    go (view → Cache (Identity (Just x)) :>>= k) = oneof
        [return Seq.empty
        ,fmap (CacheStep (encode x) <|) (go (k x))
        ]
    go (view → Choice (TreeT x) (TreeT y) :>>= k) = oneof
        [return Seq.empty
        ,fmap (ChoiceStep LeftBranch <|) (go (x >>= k))
        ,fmap (ChoiceStep RightBranch <|) (go (y >>= k))
        ]
    go (view → ProcessPendingRequests :>>= k) = go (k ())
    go _ = return Seq.empty
-- }}}

randomUniqueTreeWithHooks :: ∀ m. Monad m ⇒ Gen ((Int → m ()) → TreeT m IntSet) -- {{{
randomUniqueTreeWithHooks = fmap (($ 0) . curry) . sized $ \n → evalStateT (arb1 n 0) (-1,IntSet.empty)
  where
    arb1, arb2 :: Int → Int → StateT (Int,IntSet) Gen ((Int,Int → m ()) → TreeT m IntSet)

    arb1 n intermediate = do
        hook_id ← _1 <+= 1
        tree ← arb2 n intermediate
        return $ \args@(_,runHook) → lift (runHook hook_id) >> tree args

    arb2 0 _ = return (const mzero)
    arb2 1 intermediate = frequencyT
        [(1,return (const mzero))
        ,(1,generateUnique processPendingRequestsAndReturn intermediate)
        ,(3,generateUnique return intermediate)
        ,(2,generateUnique cache intermediate)
        ]
    arb2 n intermediate = frequencyT
        [(1,generateForNext processPendingRequestsAndReturn intermediate (arb1 n))
        ,(2,generateForNext return intermediate (arb1 n))
        ,(2,generateForNext cache intermediate (arb1 n))
        ,(4, do left_size ← lift $ choose (0,n)
                let right_size = n-left_size
                liftM2 (liftA2 mplus)
                    (arb1 left_size intermediate)
                    (arb1 right_size intermediate)
         )
        ]

    generateUnique :: -- {{{
        Monad m ⇒
        (IntSet → TreeT m IntSet) →
        Int →
        StateT (Int,IntSet) Gen ((Int,Int → m ()) → TreeT m IntSet)
    generateUnique construct intermediate = do
        observed ← use _2
        x ← lift (arbitrary `suchThat` (flip IntSet.notMember observed . (xor intermediate)))
        let final_value = x `xor` intermediate
        _2 %= IntSet.insert final_value
        return $ construct . IntSet.singleton . xor x . fst
    -- }}}

    generateForNext :: -- {{{
        Monad m ⇒
        (Int → TreeT m Int) →
        Int →
        (Int → StateT (Int,IntSet) Gen ((Int,Int → m ()) → TreeT m IntSet)) →
        StateT (Int,IntSet) Gen ((Int,Int → m ()) → TreeT m IntSet)
    generateForNext construct intermediate next = do
        x ← lift arbitrary
        let new_intermediate = x `xor` intermediate
        tree ← next new_intermediate
        return $ \(value,runHook) → do
            new_value ← construct . xor x $ value
            tree (new_value,runHook)
    -- }}}
-- }}}

randomTreeWithoutCache :: Arbitrary α ⇒ Gen (Tree α) -- {{{
randomTreeWithoutCache = sized arb
  where
    arb 0 = frequency
                [(2,result)
                ,(1,null_tree)
                ]
    arb n = frequency
                [(2,result)
                ,(1,bindToArbitrary n result)
                ,(1,bindToArbitrary n null_tree)
                ,(1,bindToArbitrary n process)
                ,(3,liftM2 mplus (arb (n `div` 2)) (arb (n `div` 2)))
                ]
    null_tree = pure mzero
    result = fmap pure arbitrary
    process = fmap processPendingRequestsAndReturn arbitrary

    bindToArbitrary n = flip (liftM2 (>>)) (arb (n-1))
-- }}}

remdups :: (Eq a) => [a] -> [a] -- {{{
remdups []  =  []
remdups (x : []) =  [x]
remdups (x : xx : xs)
 | x == xx   = remdups (x : xs)
 | otherwise = x : remdups (xx : xs)
-- }}}
-- }}}

-- Values {{{
bad_test_supervisor_actions :: SupervisorCallbacks exploration_mode worker_id m -- {{{
bad_test_supervisor_actions =
    SupervisorCallbacks
    {   broadcastProgressUpdateToWorkers =
            error "broadcastProgressUpdateToWorkers called! :-/"
    ,   broadcastWorkloadStealToWorkers =
            error "broadcastWorkloadStealToWorkers called! :-/"
    ,   receiveCurrentProgress =
            error "receiveCurrentProgress called! :-/"
    ,   sendWorkloadToWorker =
            error "sendWorkloadToWorker called! :-/"
    }
-- }}}
ignore_supervisor_actions :: Monad m ⇒ SupervisorCallbacks exploration_mode worker_id m -- {{{
ignore_supervisor_actions =
    SupervisorCallbacks
    {   broadcastProgressUpdateToWorkers = const $ return ()
    ,   broadcastWorkloadStealToWorkers = const $ return ()
    ,   receiveCurrentProgress = const $ return ()
    ,   sendWorkloadToWorker = const . const $ return ()
    }
-- }}}
endless_tree :: TreeT IO α
endless_tree = endless_tree `mplus` endless_tree
-- }}}

-- }}}

dontTestGroup :: String → [Test] → Test
dontTestGroup name = const $ testGroup name []

dontTestCase ∷ String → Assertion → Test
dontTestCase name = const $ testGroup name []

dontTestProperty ∷ Testable α ⇒ String → α → Test
dontTestProperty name = const $ testGroup name []
