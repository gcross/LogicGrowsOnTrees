-- Language extensions {{{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DoRec #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- }}}

-- Imports {{{
import Prelude hiding (catch)
import Control.Applicative
import Control.Arrow ((&&&),second)
import Control.Concurrent
import Control.Exception
import Control.Lens (_1,_2,(%=),(<+=),use)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Operational (ProgramViewT(..),view)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.State (StateT,evalStateT)
import Control.Monad.Trans.Writer

import Data.Bits
import Data.Composition ((.*))
import qualified Data.DList as DList
import Data.DList (DList)
import Data.Function
import Data.Functor.Identity
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import Data.IORef
import qualified Data.IVar as IVar
import Data.List (sort)
import Data.Maybe
import qualified Data.Map as Map
import Data.Monoid
import Data.Sequence ((<|),(|>))
import qualified Data.Sequence as Seq
import qualified Data.Serialize as Serialize
import Data.Serialize (Serialize(),encode)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Typeable
import qualified Data.UUID as UUID
import Data.UUID (UUID)
import Data.Void (absurd)
import Data.Word

import Debug.Trace (trace)

import Text.Printf

import System.IO.Unsafe
import System.Log.Logger (Priority(..),updateGlobalLogger,rootLoggerName,setLevel)
import System.Random

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import qualified Test.Framework.Providers.SmallCheck as Small
import Test.HUnit hiding (Test,Path)
import Test.QuickCheck.Arbitrary hiding ((><))
import Test.QuickCheck.Gen
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Modifiers
import Test.QuickCheck.Monadic
import Test.QuickCheck.Property hiding ((.&.),(==>))
import Test.SmallCheck ((==>))
import Test.SmallCheck.Series (Serial(..))
import Test.SmallCheck.Drivers as Small (test)

import LogicGrowsOnTrees
import LogicGrowsOnTrees.Checkpoint
import LogicGrowsOnTrees.Examples.MapColoring
import LogicGrowsOnTrees.Location
import qualified LogicGrowsOnTrees.Parallel.Adapter.Threads as Threads
import qualified LogicGrowsOnTrees.Parallel.Common.Workgroup as Workgroup
import LogicGrowsOnTrees.Parallel.ExplorationMode
import LogicGrowsOnTrees.Parallel.Main (RunOutcome(..),TerminationReason(..))
import LogicGrowsOnTrees.Parallel.Purity
import LogicGrowsOnTrees.Path
import LogicGrowsOnTrees.Parallel.Common.RequestQueue hiding (setWorkloadBufferSize)
import LogicGrowsOnTrees.Parallel.Common.Supervisor
import LogicGrowsOnTrees.Utils.PerfectTree
import LogicGrowsOnTrees.Utils.WordSum
import LogicGrowsOnTrees.Workload
import LogicGrowsOnTrees.Parallel.Common.Worker
-- }}}

-- Helpers {{{

-- Instances {{{
-- Newtypes {{{
newtype UniqueTreeT m = UniqueTree { unwrapUniqueTree :: TreeT m IntSet }
newtype NullTreeT m = NullTree { unwrapNullTree :: TreeT m IntSet }
-- }}}

-- Arbitrary {{{
instance Arbitrary BranchChoice where arbitrary = elements [LeftBranch,RightBranch]

instance Arbitrary α ⇒ Arbitrary (DList α) where -- {{{
    arbitrary = DList.fromList <$> listOf arbitrary
-- }}}

instance Arbitrary UUID where -- {{{
    arbitrary = MkGen (\r _ -> fst (random r))
-- }}}

instance (Arbitrary α, Monoid α, Serialize α, Functor m, Monad m) ⇒ Arbitrary (TreeT m α) where -- {{{
    arbitrary = fmap ($ mempty) (sized arb)
      where
        arb :: Monoid α ⇒ Int → Gen (α → TreeT m α)
        arb 0 = null
        arb 1 = frequency
            [(1,null)
            ,(3,resultPlus)
            ,(2,cachedPlus)
            ]
        arb n = frequency
            [(2,liftM2 (>=>) resultPlus (arb n))
            ,(2,liftM2 (>=>) cachedPlus (arb n))
            ,(4, do left_size ← choose (0,n)
                    let right_size = n-left_size
                    liftM2 (liftA2 mplus)
                        (arb left_size)
                        (arb right_size)
             )
            ]

        null :: Gen (α → TreeT m α)
        null = return (const mzero)

        result, cached :: Gen (TreeT m α)
        result = fmap return arbitrary
        cached = fmap cache arbitrary

        resultPlus, cachedPlus :: Monoid α ⇒ Gen (α → TreeT m α)
        resultPlus = (\x → flip fmap x . mappend) <$> result
        cachedPlus = (\x → flip fmap x . mappend) <$> cached
-- }}}

instance Monad m ⇒ Arbitrary (NullTreeT m) where -- {{{
    arbitrary = (NullTree . ($ (const $ return ()))) <$> randomNullTreeWithHooks
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
instance Serialize α ⇒ Serialize (DList α) where -- {{{
    put = Serialize.putListOf Serialize.put . DList.toList
    get = DList.fromList <$> Serialize.getListOf Serialize.get
-- }}}

instance Serialize UUID where -- {{{
    put = Serialize.putLazyByteString . UUID.toByteString
    get = fromJust . UUID.fromByteString <$> Serialize.getLazyByteString 16
-- }}}

instance Serialize (Sum Int) where -- {{{
    put = Serialize.put . getSum
    get = fmap Sum Serialize.get
-- }}}
-- }}} Serialize

-- Eq {{{
instance Eq α ⇒ Eq (DList α) where -- {{{
    (==) = (==) `on` DList.toList
-- }}}
-- }}}

-- Show {{{
instance Show α ⇒ Show (DList α) where -- {{{
    show = ("DList.fromList " ++) . show . DList.toList
-- }}}
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
type NullTree = NullTreeT Identity
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

echo :: Show α ⇒ α → α -- {{{
echo x = trace (show x) x
-- }}}

echoWithLocation :: Show α ⇒ String → α → α -- {{{
echoWithLocation label x = trace (label ++ " " ++ show x) x
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
    go2 tree = elements [(exploreTree (TreeT tree),Explored),(mempty,Unexplored)]
-- }}}

randomNullTreeWithHooks :: ∀ m. Monad m ⇒ Gen ((Int → m ()) → TreeT m IntSet) -- {{{
randomNullTreeWithHooks = fmap (($ 0) . curry) . sized $ \n → evalStateT (arb1 n 0) (-1,IntSet.empty)
  where
    arb1, arb2 :: Int → Int → StateT (Int,IntSet) Gen ((Int,Int → m ()) → TreeT m IntSet)

    arb1 n intermediate = do
        id ← _1 <+= 1
        tree ← arb2 n intermediate
        return $ \args@(_,runHook) → lift (runHook id) >> tree args

    arb2 0 _ = return (const mzero)
    arb2 1 _ = return (const mzero)
    arb2 n intermediate = frequencyT
        [(2,generateForNext return intermediate (arb1 n))
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
    go _ = return Seq.empty
-- }}}

randomUniqueTreeWithHooks :: ∀ m. Monad m ⇒ Gen ((Int → m ()) → TreeT m IntSet) -- {{{
randomUniqueTreeWithHooks = fmap (($ 0) . curry) . sized $ \n → evalStateT (arb1 n 0) (-1,IntSet.empty)
  where
    arb1, arb2 :: Int → Int → StateT (Int,IntSet) Gen ((Int,Int → m ()) → TreeT m IntSet)

    arb1 n intermediate = do
        id ← _1 <+= 1
        tree ← arb2 n intermediate
        return $ \args@(_,runHook) → lift (runHook id) >> tree args

    arb2 0 _ = return (const mzero)
    arb2 1 intermediate = frequencyT
        [(1,return (const mzero))
        ,(3,generateUnique return intermediate)
        ,(2,generateUnique cache intermediate)
        ]
    arb2 n intermediate = frequencyT
        [(2,generateForNext return intermediate (arb1 n))
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
                ,(1,null)
                ]
    arb n = frequency
                [(2,result)
                ,(1,bindToArbitrary n result)
                ,(1,bindToArbitrary n null)
                ,(3,liftM2 mplus (arb (n `div` 2)) (arb (n `div` 2)))
                ]
    null = return mzero
    result = fmap return arbitrary

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
endless_tree = endless_tree `mplus` endless_tree
-- }}}

-- }}}

main = do
    -- updateGlobalLogger rootLoggerName (setLevel DEBUG)
    defaultMain tests

tests = -- {{{
    [testGroup "test helpers" $ -- {{{
        [testProperty "UniqueTree has unique results" $ \(UniqueTree tree) → -- {{{
            let results = exploreTree (fmap (:[]) tree )
            in length results == IntSet.size (mconcat results)
         -- }}}
        ]
     -- }}}
    ,testGroup "LogicGrowsOnTrees" -- {{{
        [testGroup "Eq instance" -- {{{
            [testProperty "self" $ \(v :: Tree [()]) → v == v
            ]
         -- }}}
        ,testProperty "allFrom" $ \(x :: [Int]) → x == allFrom x
        ,testProperty "between" $ do -- {{{
            x ← choose ( 0,100) :: Gen Int
            y ← choose (50,100)
            return $ between x y == [x..y]
         -- }}}
        ,testGroup "exploreTree" -- {{{
            [testCase "return" $ exploreTree (return [()]) @?= [()]
            ,testCase "mzero" $ exploreTree (mzero :: Tree [()]) @?= []
            ,testCase "mplus" $ exploreTree (return [1::Int] `mplus` return [2]) @?= [1,2]
            ,testCase "cache" $ exploreTree (cache [42]) @?= [42::Int]
            ,testGroup "cacheMaybe" -- {{{
                [testCase "Nothing" $ exploreTree (cacheMaybe (Nothing :: Maybe [()])) @?= []
                ,testCase "Just" $ exploreTree (cacheMaybe (Just [42])) @?= [42::Int]
                ]
             -- }}}
            ,testGroup "cacheGuard" -- {{{
                [testCase "True" $ exploreTree (cacheGuard False >> return [()]) @?= []
                ,testCase "False" $ exploreTree (cacheGuard True >> return [()]) @?= [()]
                ]
             -- }}}
            ]
         -- }}}
        ,testGroup "exploreTreeT" -- {{{
            [testCase "Writer" $ -- {{{
                (runWriter . exploreTreeT $ do
                    cache [1 :: Int] >>= lift . tell
                    (lift (tell [2]) `mplus` lift (tell [3]))
                    return [42::Int]
                ) @?= ([42,42],[1,2,3])
             -- }}}
            ]
         -- }}}
        ,testGroup "exploreTreeTAndIgnoreResults" -- {{{
            [testCase "Writer" $ -- {{{
                (runWriter . exploreTreeTAndIgnoreResults $ do
                    cache [1 :: Int] >>= lift . tell
                    (lift (tell [2]) `mplus` lift (tell [3]))
                    return [42::Int]
                ) @?= ((),[1,2,3])
             -- }}}
            ]
         -- }}}
        ,testGroup "exploreTreeUntilFirst" -- {{{
            [testCase "return" $ exploreTreeUntilFirst (return 42) @=? (Just 42 :: Maybe Int)
            ,testCase "null" $ exploreTreeUntilFirst mzero @=? (Nothing :: Maybe Int)
            ,testProperty "compared to exploreTree" $ \(tree :: Tree String) →
                exploreTreeUntilFirst tree
                ==
                case exploreTree (fmap (:[]) tree) of
                    [] → Nothing
                    (x:_) → Just x
            ]
         -- }}}
        ,testGroup "exploreTreeTUntilFirst" -- {{{
            [testCase "return" $ runIdentity (exploreTreeTUntilFirst (return 42)) @=? (Just 42 :: Maybe Int)
            ,testCase "null" $ runIdentity(exploreTreeTUntilFirst mzero) @=? (Nothing :: Maybe Int)
            ,testProperty "compared to exploreTreeT" $ \(tree :: TreeT Identity String) →
                runIdentity (exploreTreeTUntilFirst tree)
                ==
                case runIdentity (exploreTreeT (fmap (:[]) tree)) of
                    [] → Nothing
                    (x:_) → Just x
            ]
         -- }}}
        ,testGroup "exploreTreeUntilFound" -- {{{
            [testProperty "compared to exploreTree" $ do
                UniqueTree tree ← arbitrary
                let solutions = exploreTree tree
                threshold ← (+1) <$> choose (0,2*IntSet.size solutions)
                return . unsafePerformIO . checkFoundAgainstThreshold threshold solutions $
                    exploreTreeUntilFound ((>= threshold) . IntSet.size) tree
            ]
         -- }}}
        ,testGroup "exploreTreeTUntilFound" -- {{{
            [testProperty "compared to exploreTreeT" $ do
                UniqueTree tree ← arbitrary
                let solutions = runIdentity (exploreTreeT tree)
                threshold ← (+1) <$> choose (0,2*IntSet.size solutions)
                return . unsafePerformIO . checkFoundAgainstThreshold threshold solutions . runIdentity $
                    exploreTreeTUntilFound ((>= threshold) . IntSet.size) tree
            ]
         -- }}}
        ]
     -- }}}
    ,testGroup "LogicGrowsOnTrees.Checkpoint" -- {{{
        [testGroup "contextFromCheckpoint" -- {{{
            [testProperty "cache" $ \(checkpoint :: Checkpoint) (i :: Int) → -- {{{
                checkpointFromContext (Seq.singleton (CacheContextStep (encode i))) checkpoint
                ==
                (simplifyCheckpointRoot $ CachePoint (encode i) checkpoint)
             -- }}}
            ,testProperty "left branch" $ \(inner_checkpoint :: Checkpoint) (other_tree :: Tree [()]) (other_checkpoint :: Checkpoint) → -- {{{
                (checkpointFromContext (Seq.singleton (LeftBranchContextStep other_checkpoint other_tree)) inner_checkpoint)
                ==
                (simplifyCheckpointRoot $ ChoicePoint inner_checkpoint other_checkpoint)
             -- }}}
            ,testProperty "right branch" $ \(checkpoint :: Checkpoint) → -- {{{
                checkpointFromContext (Seq.singleton RightBranchContextStep) checkpoint
                ==
                (simplifyCheckpointRoot $ ChoicePoint Explored checkpoint)
             -- }}}
            ,testProperty "empty" $ \(checkpoint :: Checkpoint) → -- {{{
                checkpointFromContext Seq.empty checkpoint == checkpoint
             -- }}}
            ]
         -- }}}
        ,testProperty "invertCheckpoint" $ \(tree :: Tree (Set UUID)) → -- {{{
            randomCheckpointForTree tree >>= \(partial_result,checkpoint) → return $
                partial_result == exploreTreeStartingFromCheckpoint (invertCheckpoint checkpoint) tree
         -- }}}
        ,testGroup "Monoid instance" -- {{{
            [testProperty "product results in intersection of solutions" $ \(UniqueTree tree) → do -- {{{
                (_,checkpoint1) ← randomCheckpointForTree tree
                (_,checkpoint2) ← randomCheckpointForTree tree
                let checkpoint3 = checkpoint1 `mappend` checkpoint2
                    solutions1 = exploreTreeStartingFromCheckpoint checkpoint1 tree
                    solutions2 = exploreTreeStartingFromCheckpoint checkpoint2 tree
                    solutions3 = exploreTreeStartingFromCheckpoint checkpoint3 tree
                return $ solutions3 == solutions1 `IntSet.intersection` solutions2
             -- }}}
            ,testCase "throws the correct exceptions" $ -- {{{
                mapM_ (\(x,y) →
                    try (
                        evaluate (x `mappend` y)
                        >>
                        assertFailure (show x ++ " and " ++ show y ++ " were not recognized as being inconsistent")
                    )
                    >>=
                    assertEqual "the thrown exception was incorrect" (Left $ InconsistentCheckpoints x y)
                )
                [((CachePoint (encode (42 :: Int)) Unexplored),(CachePoint (encode (42 :: Integer)) Unexplored))
                ,((ChoicePoint Unexplored Unexplored),CachePoint (encode (42 :: Int)) Unexplored)
                ]
             -- }}}
            ,testProperty "unit element laws" $ \(checkpoint :: Checkpoint) → -- {{{
                (mempty `mappend` checkpoint == checkpoint) && (checkpoint `mappend` mempty == checkpoint)
             -- }}}
            ]
         -- }}}
        ,testProperty "stepThroughTreeStartingFromCheckpoint" $ do -- {{{
            UniqueTree tree ← arbitrary
            (partial_result,checkpoint) ← randomCheckpointForTree tree
            let go state@ExplorationTState{..} current_result =
                    exploreTreeStartingFromCheckpoint
                        (invertCheckpoint (checkpointFromExplorationState state))
                        tree
                    ==
                    current_result
                    &&
                    case stepThroughTreeStartingFromCheckpoint state of
                        (Just result,Nothing) → exploreTree tree == current_result <> result
                        (Nothing,Nothing) → exploreTree tree == current_result
                        (Just result,Just new_state) → go new_state (current_result <> result)
                        (Nothing,Just new_state) → go new_state current_result
            return $ go (initialExplorationState checkpoint tree) partial_result
         -- }}}
        ,testProperty "exploreTreeStartingFromCheckpoint" $ \(UniqueTree tree) → -- {{{
            randomCheckpointForTree tree >>= \(partial_result,checkpoint) → return $
                exploreTree tree ==
                    mappend partial_result (exploreTreeStartingFromCheckpoint checkpoint tree)
         -- }}}
        ,testProperty "exploreTreeUntilFirstStartingFromCheckpoint" $ \(UniqueTree tree) → -- {{{
            randomCheckpointForTree tree >>= \(_,checkpoint) → return $
                let all_results = exploreTreeStartingFromCheckpoint checkpoint tree
                    maybe_first_result = exploreTreeUntilFirstStartingFromCheckpoint checkpoint tree
                in case maybe_first_result of
                    Nothing → IntSet.null all_results
                    Just result → IntSet.size result == 1 && IntSet.member (IntSet.findMin result) all_results
         -- }}}
        ,testProperty "exploreTreeTUntilFirstStartingFromCheckpoint" $ \(UniqueTree tree) → -- {{{
            randomCheckpointForTree tree >>= \(_,checkpoint) → return $
                let all_results = exploreTreeStartingFromCheckpoint checkpoint tree
                    maybe_first_result = runIdentity $ exploreTreeTUntilFirstStartingFromCheckpoint checkpoint tree
                in case maybe_first_result of
                    Nothing → IntSet.null all_results
                    Just result → IntSet.size result == 1 && IntSet.member (IntSet.findMin result) all_results
         -- }}}
        ,testProperty "exploreTreeUntilFoundStartingFromCheckpoint" $ do -- {{{
            UniqueTree tree ← arbitrary
            (_,checkpoint) ← randomCheckpointForTree tree
            let solutions = exploreTreeStartingFromCheckpoint checkpoint tree
            threshold ← (+1) <$> choose (0,2*IntSet.size solutions)
            return . unsafePerformIO . checkFoundAgainstThreshold threshold solutions $
                exploreTreeUntilFoundStartingFromCheckpoint ((>= threshold) . IntSet.size) checkpoint tree
         -- }}}
        ,testProperty "exploreTreeTUntilFoundStartingFromCheckpoint" $ do -- {{{
            UniqueTree tree ← arbitrary
            (_,checkpoint) ← randomCheckpointForTree tree
            let solutions = exploreTreeStartingFromCheckpoint checkpoint tree
            threshold ← (+1) <$> choose (0,2*IntSet.size solutions)
            return . unsafePerformIO . checkFoundAgainstThreshold threshold solutions . runIdentity $
                exploreTreeTUntilFoundStartingFromCheckpoint ((>= threshold) . IntSet.size) checkpoint tree
          -- }}}
        ]
     -- }}}
    ,testGroup "LogicGrowsOnTrees.Examples" -- {{{
        [testProperty name $ do
            number_of_colors ← choose (2,5)
            number_of_countries ← choose (3,7)
            neighbor_probability ← choose (0,1::Float)
            neighbors ← fmap (concat . concat) $
                forM [1..number_of_countries] $ \x →
                    forM [x+1..number_of_countries] $ \y → do
                        outcome ← choose (0,1)
                        return $
                            if outcome > neighbor_probability
                                then [(x,y),(y,x)]
                                else []
            let solutions =
                    computeSolutions
                        number_of_colors
                        number_of_countries
                        (\x y → (x,y) `elem` neighbors)
            morallyDubiousIOProperty $ do
                forM_ solutions $ \solution →
                    forM_ solution $ \(country_1,color_1) →
                        forM_ solution $ \(country_2,color_2) →
                            when ((country_1,country_2) `elem` neighbors) $
                                assertBool "neighbors have different colors" $ color_1 /= color_2
                let correct_count = sum $ do
                        solution ← zip [1..] <$> replicateM (fromIntegral number_of_countries) [1..number_of_colors]
                        forM_ solution $ \(country_1,color_1) →
                            forM_ solution $ \(country_2,color_2) →
                                when ((country_1,country_2) `elem` neighbors) $
                                    guard $ color_1 /= color_2
                        return 1
                computeCount number_of_colors solutions @?= correct_count
                return True
        | (name,computeSolutions,computeCount) ←
            [("coloringSolutions",coloringSolutions,curry (fromIntegral . length . snd))
            ,("coloringUniqueSolutions",coloringUniqueSolutions,
                \number_of_colors →
                    sum
                    .
                    map (\solution →
                        let number_of_colors_used = maximum . fmap snd $ solution
                        in product [number_of_colors-number_of_colors_used+1..number_of_colors]
                    )
             )
            ]
        ]
     -- }}}
    ,testGroup "LogicGrowsOnTrees.Location" -- {{{
        [testProperty "branchingFromLocation . labelFromBranching = id" $ -- {{{
            liftA2 (==)
                (branchingFromLocation . labelFromBranching)
                id
         -- }}}
        ,testProperty "labelFromBranching . branchingFromLocation = id" $ -- {{{
            liftA2 (==)
                (labelFromBranching . branchingFromLocation)
                id
         -- }}}
        ,testGroup "Monoid instance" -- {{{
            [testProperty "equivalent to concatenation of branchings" $ \(parent_branching :: [BranchChoice]) (child_branching :: [BranchChoice]) → -- {{{
                labelFromBranching parent_branching `mappend` labelFromBranching child_branching
                ==
                labelFromBranching (parent_branching `mappend` child_branching)
             -- }}}
            ,testProperty "obeys monoid laws" $ -- {{{
                liftA2 (&&)
                    (liftA2 (==) id (`mappend` (mempty :: Location)))
                    (liftA2 (==) id ((mempty :: Location) `mappend`))
             -- }}}
            ]
         -- }}}
        ,testProperty "Ord instance of Location equivalent to Ord of branching" $ \a b → -- {{{
            (compare `on` branchingFromLocation) a b == compare a b
         -- }}}
        ,testGroup "exploreTreeWithLocations" -- {{{
            [testProperty "same result as exploreTree" $ \(tree :: Tree [()]) →
                 exploreTree ((:[]) <$> tree) == (solutionResult <$> exploreTreeWithLocations tree)
            ]
         -- }}}
        ,testGroup "sendTreeDownLocation" -- {{{
            [testProperty "same result as walking down path" $ do -- {{{
                tree :: Tree Int ← randomTreeWithoutCache
                path ← randomPathForTree tree
                let label = labelFromPath path
                return $
                    sendTreeDownPath path tree
                    ==
                    sendTreeDownLocation label tree
             -- }}}
            ]
         -- }}}
        ,testProperty "exploreLocatableTree" $ -- {{{
            let gen _ 0 = return mzero
                gen label 1 = return (All . (== label) <$> getLocation)
                gen label n = do
                    left_size ← choose (0,n)
                    let right_size = n-left_size
                    left ← gen (leftBranchOf label) left_size
                    right ← gen (rightBranchOf label) right_size
                    return $ left `mplus` right
            in getAll . exploreLocatableTree <$> sized (gen rootLocation)
         -- }}}
        ]
     -- }}}
    ,testGroup "LogicGrowsOnTrees.Parallel.Adapter.Threads" $ -- {{{
        [testGroup "FirstMode" -- {{{
            [testCase "two threads, one blocked" $ do -- {{{
                RunOutcome _ termination_reason ←
                    Threads.exploreTreeIOUntilFirst
                        (void . Workgroup.changeNumberOfWorkers . const . return $ 2)
                        (liftIO (threadDelay 1) >> endless_tree
                         `mplus`
                         return ()
                        )
                termination_reason @?= Completed (Just (Progress (ChoicePoint Unexplored Explored) ()))
             -- }}}
            ]
         -- }}}
        ,testGroup "FoundModeUsingPull" -- {{{
            [testCase "many threads with combined final result but none finish" $ do -- {{{
                RunOutcome _ termination_reason ←
                    Threads.exploreTreeIOUntilFoundUsingPull
                        ((== 2) . length)
                        (void . Workgroup.changeNumberOfWorkers . const . return $ 4)
                        ((return [1] `mplus` endless_tree) `mplus` (return [2] `mplus` endless_tree))
                case termination_reason of
                    Completed (Right (Progress _ result)) → sort result @?= [1,2]
                    _ → fail $ "got incorrect result: " ++ show termination_reason
             -- }}}
            ]
         -- }}}
        ,testGroup "FoundModeUsingPush" -- {{{
            [testCase "two threads with combined final result but none finish" $ do -- {{{
                RunOutcome _ termination_reason ←
                    Threads.exploreTreeIOUntilFoundUsingPush
                        ((== 2) . length)
                        (void . Workgroup.changeNumberOfWorkers . const . return $ 2)
                        ((return [1] `mplus` endless_tree) `mplus` (return [2] `mplus` endless_tree))
                case termination_reason of
                    Completed (Right (Progress _ result)) → sort result @?= [1,2]
                    _ → fail $ "got incorrect result: " ++ show termination_reason
             -- }}}
            ]
         -- }}}
        ,plusTestOptions (mempty {topt_maximum_generated_tests = Just 10}) $ testGroup "stress tests" $ -- {{{
            let extractResult (RunOutcome _ termination_reason) = -- {{{
                    case termination_reason of
                        Aborted _ → error "prematurely aborted"
                        Completed result → return result
                        Failure _ message → error message
                -- }}}
                insertHooks cleared_flags_mvar request_queue = ($ \id → liftIO $ do -- {{{
                    threadDelay 10
                    mvar ← modifyMVar cleared_flags_mvar $ \cleared_flags →
                        case Map.lookup id cleared_flags of
                            Nothing → do
                                mvar ← newEmptyMVar
                                writeChan request_queue mvar
                                return (Map.insert id mvar cleared_flags,mvar)
                            Just mvar → return (cleared_flags,mvar)
                    readMVar mvar
                 ) -- }}}
                receiveProgressInto progresses_ref progress = atomicModifyIORef progresses_ref ((progress:) &&& const ())
                respondToRequests request_queue generateNoise progresses_ref = do -- {{{
                    _ ← Workgroup.changeNumberOfWorkers (const . return $ 1)
                    forever $ do
                        liftIO $ threadDelay 10
                        mvar ← liftIO $ readChan request_queue
                        liftIO $ threadDelay 10
                        generateNoise $ receiveProgressInto progresses_ref
                        liftIO $ threadDelay 10
                        liftIO $ putMVar mvar ()
                -- }}}
                oneThreadNoise receiveProgress = liftIO (randomRIO (0,1::Int)) >>= \i → case i of -- {{{
                    0 → void $ do
                         Workgroup.changeNumberOfWorkers (return . (const 0))
                         Workgroup.changeNumberOfWorkers (return . (const 1))
                    1 → void $ requestProgressUpdateAsync receiveProgress
                -- }}}
                twoThreadsNoise receiveProgress = liftIO (randomRIO (0,1::Int)) >>= \i → case i of -- {{{
                    0 → void $ Workgroup.changeNumberOfWorkers (return . (\i → 3-i))
                    1 → void $ requestProgressUpdateAsync receiveProgress
                -- }}}
                manyThreadsNoise receiveProgress = liftIO (randomRIO (0,2::Int)) >>= \i → case i of -- {{{
                    0 → void $ Workgroup.changeNumberOfWorkers (return . (\i → if i > 1 then i-1 else i))
                    1 → void $ Workgroup.changeNumberOfWorkers (return . (+1))
                    2 → void $ requestProgressUpdateAsync receiveProgress
                -- }}}
            in
            [testGroup "AllMode" $ -- {{{
                let runTest generateNoise = randomUniqueTreeWithHooks >>= \constructTree → morallyDubiousIOProperty $ do
                        cleared_flags_mvar ← newMVar mempty
                        request_queue ← newChan
                        progresses_ref ← newIORef []
                        result ←
                            (Threads.exploreTreeIO
                                (respondToRequests request_queue generateNoise progresses_ref)
                                (insertHooks cleared_flags_mvar request_queue constructTree)
                            ) >>= extractResult
                        let tree = constructTree (const $ return ())
                        correct_result ← exploreTreeT tree
                        result @?= correct_result
                        (remdups <$> readIORef progresses_ref) >>= mapM_ (\(Progress checkpoint result) → do
                            exploreTreeTStartingFromCheckpoint (invertCheckpoint checkpoint) tree >>= (@?= result)
                            exploreTreeTStartingFromCheckpoint checkpoint tree >>= (@?= correct_result ) . mappend result
                         )
                        return True
              in
              [testProperty "one thread" . runTest $ oneThreadNoise
              ,testProperty "two threads" . runTest $ twoThreadsNoise
              ,testProperty "many threads" . runTest $ manyThreadsNoise
              ]
             -- }}}
            ,testGroup "FirstMode" $ -- {{{
                let runTest generator generateNoise = generator >>= \constructTree → morallyDubiousIOProperty $ do
                        cleared_flags_mvar ← newMVar mempty
                        request_queue ← newChan
                        progresses_ref ← newIORef []
                        maybe_result ←
                            (Threads.exploreTreeIOUntilFirst
                                (respondToRequests request_queue generateNoise progresses_ref)
                                (insertHooks cleared_flags_mvar request_queue constructTree)
                            ) >>= extractResult
                        let tree = constructTree (const $ return ())
                        correct_results ← exploreTreeT tree
                        case maybe_result of
                            Nothing → assertBool "solutions were missed" (IntSet.null correct_results)
                            Just (Progress checkpoint result) → do
                                IntSet.size result @?= 1
                                assertBool "solution was not valid" $ result `IntSet.isSubsetOf` correct_results
                                exploreTreeTStartingFromCheckpoint (invertCheckpoint checkpoint) tree >>= (@=? result)
                                exploreTreeTStartingFromCheckpoint checkpoint tree >>= (@=? IntSet.difference correct_results result)
                        (remdups <$> readIORef progresses_ref) >>= mapM_ (\checkpoint → do
                            exploreTreeTUntilFirstStartingFromCheckpoint (invertCheckpoint checkpoint) tree >>= (@?= Nothing)
                         )
                        return True
                    testGroupUsingGenerator name generator = testGroup name $
                        [testProperty "one thread" . runTest generator $ oneThreadNoise
                        ,testProperty "two threads" . runTest generator $ twoThreadsNoise
                        ,testProperty "many threads" . runTest generator $ manyThreadsNoise
                        ]
                in [testGroupUsingGenerator "with solutions" randomUniqueTreeWithHooks
                   ,testGroupUsingGenerator "without solutions" randomNullTreeWithHooks
                   ]
             -- }}}
            ,testGroup "FoundModeUsingPull" $ -- {{{
                let runTest generator generateNoise = generator >>= \constructTree → morallyDubiousIOProperty $ do
                        let tree = constructTree (const $ return ())
                        all_results ← exploreTreeT tree
                        number_of_results_to_find ← randomRIO (1,2*IntSet.size all_results)
                        cleared_flags_mvar ← newMVar mempty
                        request_queue ← newChan
                        progresses_ref ← newIORef []
                        result ←
                            (Threads.exploreTreeIOUntilFoundUsingPull
                                ((>= number_of_results_to_find) . IntSet.size)
                                (respondToRequests request_queue generateNoise progresses_ref)
                                (insertHooks cleared_flags_mvar request_queue constructTree)
                            ) >>= extractResult
                        case result of
                            Left incomplete_result → do
                                assertBool "result is not smaller than desired" $ IntSet.size incomplete_result < number_of_results_to_find
                                assertEqual "incomplete result matches all results" incomplete_result all_results
                            Right (Progress checkpoint final_result) → do
                                assertBool "final result is at least as large as desired" $ IntSet.size final_result >= number_of_results_to_find
                                assertBool "final result was not valid" $ final_result `IntSet.isSubsetOf` all_results
                                exploreTreeTStartingFromCheckpoint (invertCheckpoint checkpoint) tree
                                    >>= assertEqual "final results together do not match all results covered by the checkpoint" final_result
                                exploreTreeTStartingFromCheckpoint checkpoint tree
                                    >>= assertEqual "all results minus final results do not match remaining results" (IntSet.difference all_results final_result)
                        (remdups <$> readIORef progresses_ref) >>= mapM_ (\(Progress checkpoint result) → do
                            exploreTreeTStartingFromCheckpoint (invertCheckpoint checkpoint) tree >>= (@?= result)
                            exploreTreeTStartingFromCheckpoint checkpoint tree >>= (@?= all_results) . mappend result
                         )
                        return True
                    testGroupUsingGenerator name generator = testGroup name $
                        [testProperty "one thread" . runTest generator $ oneThreadNoise
                        ,testProperty "two threads" . runTest generator $ twoThreadsNoise
                        ,testProperty "many threads" . runTest generator $ manyThreadsNoise
                        ]
                in [testGroupUsingGenerator "with solutions" randomUniqueTreeWithHooks
                   ,testGroupUsingGenerator "without solutions" randomNullTreeWithHooks
                   ]
             -- }}}
            ,testGroup "FoundModeUsingPush" $ -- {{{
                let runTest generator generateNoise = generator >>= \constructTree → morallyDubiousIOProperty $ do
                        let tree = constructTree (const $ return ())
                        all_results ← exploreTreeT tree
                        number_of_results_to_find ← randomRIO (1,2*IntSet.size all_results)
                        cleared_flags_mvar ← newMVar mempty
                        request_queue ← newChan
                        progresses_ref ← newIORef []
                        result ←
                            (Threads.exploreTreeIOUntilFoundUsingPush
                                ((>= number_of_results_to_find) . IntSet.size)
                                (respondToRequests request_queue generateNoise progresses_ref)
                                (insertHooks cleared_flags_mvar request_queue constructTree)
                            ) >>= extractResult
                        case result of
                            Left incomplete_result → do
                                assertBool "result is not smaller than desired" $ IntSet.size incomplete_result < number_of_results_to_find
                                assertEqual "incomplete result matches all results" incomplete_result all_results
                            Right (Progress checkpoint final_result) → do
                                assertBool "result is at least as large as desired" $ IntSet.size final_result >= number_of_results_to_find
                                assertBool "final result was not valid" $ final_result `IntSet.isSubsetOf` all_results
                                exploreTreeTStartingFromCheckpoint (invertCheckpoint checkpoint) tree
                                    >>= assertEqual "both returned results together do not match all results covered by the checkpoint" final_result
                                exploreTreeTStartingFromCheckpoint checkpoint tree
                                    >>= assertEqual "all results minus return results do not match remaining results" (IntSet.difference all_results final_result)
                        (remdups <$> readIORef progresses_ref) >>= mapM_ (\(Progress checkpoint result) → do
                            exploreTreeTStartingFromCheckpoint (invertCheckpoint checkpoint) tree >>= (@?= result)
                            exploreTreeTStartingFromCheckpoint checkpoint tree >>= (@?= all_results) . mappend result
                         )
                        return True
                    testGroupUsingGenerator name generator = testGroup name $
                        [testProperty "one thread" . runTest generator $ oneThreadNoise
                        ,testProperty "two threads" . runTest generator $ twoThreadsNoise
                        ,testProperty "many threads" . runTest generator $ manyThreadsNoise
                        ]
                in [testGroupUsingGenerator "with solutions" randomUniqueTreeWithHooks
                   ,testGroupUsingGenerator "without solutions" randomNullTreeWithHooks
                   ]
             -- }}}
            ]
         -- }}}
        ]
     -- }}}
    ,testGroup "LogicGrowsOnTrees.Parallel.Common.RequestQueue" -- {{{
        [testCase "kills all controller threads" $ do -- {{{
            starts@[a,b,c,d] ← replicateM 4 newEmptyMVar
            vars@[w,x,y,z] ← replicateM 4 newEmptyMVar
            request_queue ← newRequestQueue
            forkControllerThread request_queue . liftIO $ do
                try (putMVar a () >> forever yield) >>= putMVar w
            forkControllerThread request_queue $ do
                fork (do
                    fork . liftIO $ try (putMVar b () >> forever yield) >>= putMVar x
                    liftIO $ try (putMVar c () >> forever yield) >>= putMVar y
                  :: RequestQueueReader (AllMode ()) () IO ()
                 )
                liftIO $ try (putMVar d () >> forever yield) >>= putMVar z
            forM_ starts $ takeMVar
            killControllerThreads request_queue
            forM_ vars $ \var → do
                value ← takeMVar var
                case value of
                    Right () → assertFailure "Thread did not infinitely loop."
                    Left e →
                        case fromException e of
                            Just ThreadKilled → return ()
                            _ → assertFailure $ "Unexpected exception: " ++ show e
         -- }}}
        ]
     -- }}}
    ,testGroup "LogicGrowsOnTrees.Parallel.Common.Supervisor" -- {{{
        [testCase "immediately abort" $ do -- {{{
            SupervisorOutcome{..} ← runSupervisor AllMode bad_test_supervisor_actions (UnrestrictedProgram abortSupervisor)
            supervisorTerminationReason @?= SupervisorAborted (Progress Unexplored ())
            supervisorRemainingWorkers @?= ([] :: [Int])
         -- }}}
        ,testCase "failure" $ do -- {{{
            SupervisorOutcome{..} ← runUnrestrictedSupervisor AllMode bad_test_supervisor_actions (receiveWorkerFailure () "FAIL" :: ∀ α. SupervisorMonad (AllMode ()) () IO α)
            supervisorTerminationReason @?= SupervisorFailure mempty () "FAIL"
            supervisorRemainingWorkers @?= []
         -- }}}
        ,testGroup "adding and removing workers" -- {{{
            [testGroup "without workload buffer" -- {{{
                [testCase "add one worker then abort" $ do -- {{{
                    (maybe_workload_ref,actions) ← addAcceptOneWorkloadAction bad_test_supervisor_actions
                    SupervisorOutcome{..} ← runUnrestrictedSupervisor AllMode actions $ do
                        enableSupervisorDebugMode
                        setWorkloadBufferSize 0
                        addWorker ()
                        abortSupervisor
                    supervisorTerminationReason @?= SupervisorAborted (Progress Unexplored ())
                    supervisorRemainingWorkers @?= [()]
                    readIORef maybe_workload_ref >>= (@?= Just ((),entire_workload))
                 -- }}}
                ,testCase "add then remove one worker then abort" $ do -- {{{
                    (maybe_workload_ref,actions) ← addAcceptOneWorkloadAction bad_test_supervisor_actions
                    SupervisorOutcome{..} ← runUnrestrictedSupervisor AllMode actions $ do
                        enableSupervisorDebugMode
                        setWorkloadBufferSize 0
                        addWorker ()
                        removeWorker ()
                        abortSupervisor
                    supervisorTerminationReason @?= SupervisorAborted (Progress Unexplored ())
                    supervisorRemainingWorkers @?= []
                    readIORef maybe_workload_ref >>= (@?= Just ((),entire_workload)) 
                 -- }}}
                ,testCase "add then remove then add one worker then abort" $ do -- {{{
                    (maybe_workload_ref,actions) ← addAcceptMultipleWorkloadsAction bad_test_supervisor_actions
                    SupervisorOutcome{..} ← runUnrestrictedSupervisor AllMode actions $ do
                        enableSupervisorDebugMode
                        setWorkloadBufferSize 0
                        addWorker 1
                        removeWorker 1
                        addWorker 2
                        abortSupervisor
                    supervisorTerminationReason @?= SupervisorAborted (Progress Unexplored ())
                    supervisorRemainingWorkers @?= [2::Int]
                    readIORef maybe_workload_ref >>= (@?= [(1,entire_workload),(2,entire_workload)]) 
                 -- }}}
                ,testCase "add two workers then remove first worker then abort" $ do -- {{{
                    (maybe_workload_ref,actions1) ← addAcceptMultipleWorkloadsAction bad_test_supervisor_actions
                    (broadcast_ids_list_ref,actions2) ← addAppendWorkloadStealBroadcastIdsAction actions1
                    SupervisorOutcome{..} ← runUnrestrictedSupervisor AllMode actions2 $ do
                        enableSupervisorDebugMode
                        setWorkloadBufferSize 0
                        addWorker 1
                        addWorker 2
                        removeWorker 1
                        abortSupervisor
                    supervisorTerminationReason @?= SupervisorAborted (Progress Unexplored ())
                    supervisorRemainingWorkers @?= [2::Int]
                    readIORef maybe_workload_ref >>= (@?= [(1,entire_workload),(2,entire_workload)])
                    readIORef broadcast_ids_list_ref >>= (@?= [[1]])
                 -- }}}
                ,testProperty "add then remove many workers then abort" $ do -- {{{
                    (NonEmpty worker_ids_to_add :: NonEmptyList UUID) ← arbitrary
                    worker_ids_to_remove ←
                       (fmap concat
                        $
                        forM (tail worker_ids_to_add)
                        $
                        \worker_id → do
                            should_remove ← arbitrary
                            if should_remove
                                then return [worker_id]
                                else return []
                        ) >>= shuffle
                    let worker_ids_left = Set.toAscList $ Set.fromList worker_ids_to_add `Set.difference` Set.fromList worker_ids_to_remove 
    
                    monadicIO . run $ do
                        (maybe_workload_ref,actions_1) ← addAcceptOneWorkloadAction bad_test_supervisor_actions
                        (broadcast_ids_list_ref,actions_2) ← addAppendWorkloadStealBroadcastIdsAction actions_1
                        SupervisorOutcome{..} ← runUnrestrictedSupervisor AllMode actions_2 $ do
                            enableSupervisorDebugMode
                            setWorkloadBufferSize 0
                            mapM_ addWorker worker_ids_to_add
                            mapM_ removeWorker worker_ids_to_remove
                            abortSupervisor
                        supervisorTerminationReason @?= SupervisorAborted (Progress Unexplored ())
                        sort supervisorRemainingWorkers @?= worker_ids_left 
                        readIORef maybe_workload_ref >>= (@?= Just (head worker_ids_to_add,entire_workload))
                        readIORef broadcast_ids_list_ref >>= (@?= if (null . tail) worker_ids_to_add then [] else [[head worker_ids_to_add]])
                 -- }}}
                ]
             -- }}}
            ,testGroup "with workload buffer" -- {{{
                [testCase "add one worker then abort" $ do -- {{{
                    (maybe_workload_ref,actions1) ← addAcceptOneWorkloadAction bad_test_supervisor_actions
                    (broadcasts_ref,actions2) ← addAppendWorkloadStealBroadcastIdsAction actions1
                    SupervisorOutcome{..} ← runUnrestrictedSupervisor AllMode actions2 $ do
                        enableSupervisorDebugMode
                        addWorker ()
                        abortSupervisor
                    supervisorTerminationReason @?= SupervisorAborted (Progress Unexplored ())
                    supervisorRemainingWorkers @?= [()]
                    readIORef maybe_workload_ref >>= (@?= Just ((),entire_workload))
                    readIORef broadcasts_ref >>= (@?= [[()]])
                 -- }}}
                ,testCase "add then remove one worker then abort" $ do -- {{{
                    (maybe_workload_ref,actions1) ← addAcceptOneWorkloadAction bad_test_supervisor_actions
                    (broadcasts_ref,actions2) ← addAppendWorkloadStealBroadcastIdsAction actions1
                    SupervisorOutcome{..} ← runUnrestrictedSupervisor AllMode actions2 $ do
                        enableSupervisorDebugMode
                        addWorker ()
                        removeWorker ()
                        abortSupervisor
                    supervisorTerminationReason @?= SupervisorAborted (Progress Unexplored ())
                    supervisorRemainingWorkers @?= []
                    readIORef maybe_workload_ref >>= (@?= Just ((),entire_workload)) 
                    readIORef broadcasts_ref >>= (@?= [[()]])
                 -- }}}
                ,testCase "add then remove then add one worker then abort" $ do -- {{{
                    (maybe_workload_ref,actions1) ← addAcceptMultipleWorkloadsAction bad_test_supervisor_actions
                    (broadcasts_ref,actions2) ← addAppendWorkloadStealBroadcastIdsAction actions1
                    SupervisorOutcome{..} ← runUnrestrictedSupervisor AllMode actions2 $ do
                        enableSupervisorDebugMode
                        addWorker 1
                        removeWorker 1
                        addWorker 2
                        abortSupervisor
                    supervisorTerminationReason @?= SupervisorAborted (Progress Unexplored ())
                    supervisorRemainingWorkers @?= [2::Int]
                    readIORef maybe_workload_ref >>= (@?= [(1,entire_workload),(2,entire_workload)]) 
                    readIORef broadcasts_ref >>= (@?= [[1],[2]])
                 -- }}}
                ]
             -- }}}
            ]
         -- }}}
        ,testGroup "progress updates" -- {{{
            [testCase "request progress update when no workers present" $ do -- {{{
                (maybe_progress_ref,actions) ← addReceiveCurrentProgressAction bad_test_supervisor_actions
                SupervisorOutcome{..} ← runUnrestrictedSupervisor AllMode actions $ do
                    enableSupervisorDebugMode
                    setWorkloadBufferSize 0
                    performGlobalProgressUpdate
                    abortSupervisor
                supervisorTerminationReason @?= SupervisorAborted (Progress Unexplored ())
                supervisorRemainingWorkers @?= ([] :: [()])
                readIORef maybe_progress_ref >>= (@?= Just (Progress Unexplored ()))
             -- }}}
            ,testProperty "request progress update when all active workers present leave" $ do -- {{{
                number_of_active_workers ← choose (1,10 :: Int)
                number_of_inactive_workers ← choose (0,10)
                let active_workers = [0..number_of_active_workers-1]
                    inactive_workers = [101..101+number_of_inactive_workers-1]
                monadicIO . run $ do
                    (maybe_progress_ref,actions1) ← addReceiveCurrentProgressAction bad_test_supervisor_actions
                    (broadcast_ids_list_ref,actions2) ← addAppendProgressBroadcastIdsAction actions1
                    (workload_steal_ids_ref,actions3) ← addSetWorkloadStealBroadcastIdsAction actions2
                    let actions4 = ignoreAcceptWorkloadAction $ actions3
                    let progress = Progress Unexplored (Sum 0)
                    SupervisorOutcome{..} ← runUnrestrictedSupervisor AllMode actions4 $ do
                        setWorkloadBufferSize 0
                        addWorker 0
                        forM_ (zip [0..] (tail active_workers)) $ \(prefix_count,worker_id) → do
                            addWorker worker_id
                            [worker_to_steal_from] ← liftIO $ readIORef workload_steal_ids_ref
                            let remaining_workload = Workload (Seq.replicate (prefix_count+1) (ChoiceStep LeftBranch)) Unexplored
                            let stolen_workload = Workload (Seq.replicate (prefix_count) (ChoiceStep LeftBranch) |> (ChoiceStep RightBranch)) Unexplored
                            receiveStolenWorkload worker_to_steal_from $ Just (StolenWorkload (ProgressUpdate mempty remaining_workload) stolen_workload)
                        mapM_ addWorker inactive_workers
                        performGlobalProgressUpdate
                        mapM_ removeWorker active_workers
                        abortSupervisor
                    supervisorTerminationReason @?= SupervisorAborted progress
                    supervisorRemainingWorkers @?= inactive_workers
                    readIORef broadcast_ids_list_ref >>= (@?= [active_workers])
                    readIORef maybe_progress_ref >>= (@?= Just progress)
             -- }}}
            ,testCase "request and receive Just progress update when one worker present" $ do -- {{{
                (maybe_progress_ref,actions1) ← addReceiveCurrentProgressAction bad_test_supervisor_actions
                (broadcast_ids_list_ref,actions2) ← addAppendProgressBroadcastIdsAction actions1
                let actions3 = ignoreAcceptWorkloadAction actions2
                let progress = Progress (ChoicePoint Unexplored Unexplored) (Sum 1)
                SupervisorOutcome{..} ← runUnrestrictedSupervisor AllMode actions3 $ do
                    enableSupervisorDebugMode
                    setWorkloadBufferSize 0
                    addWorker ()
                    performGlobalProgressUpdate
                    receiveProgressUpdate () $ ProgressUpdate progress entire_workload
                    abortSupervisor
                supervisorTerminationReason @?= SupervisorAborted progress
                supervisorRemainingWorkers @?= [()]
                readIORef maybe_progress_ref >>= (@?= Just progress)
                readIORef broadcast_ids_list_ref >>= (@?= [[()]])
             -- }}}
            ,testCase "request and receive progress update when active and inactive workers present" $ do -- {{{
                (maybe_progress_ref,actions1) ← addReceiveCurrentProgressAction bad_test_supervisor_actions
                (broadcast_ids_list_ref,actions2) ← addAppendProgressBroadcastIdsAction actions1
                let actions3 = ignoreAcceptWorkloadAction . ignoreWorkloadStealAction $ actions2
                let progress = Progress (ChoicePoint Unexplored Unexplored) (Sum 1)
                SupervisorOutcome{..} ← runUnrestrictedSupervisor AllMode actions3 $ do
                    enableSupervisorDebugMode
                    setWorkloadBufferSize 0
                    addWorker (1 :: Int)
                    addWorker (2 :: Int)
                    performGlobalProgressUpdate
                    receiveProgressUpdate 1 $ ProgressUpdate progress entire_workload
                    abortSupervisor
                supervisorTerminationReason @?= SupervisorAborted progress
                supervisorRemainingWorkers @?= [1,2]
                readIORef maybe_progress_ref >>= (@?= Just progress)
                readIORef broadcast_ids_list_ref >>= (@?= [[1]])
             -- }}}
            ]
         -- }}}
        ,testGroup "workload steals" -- {{{
            [testCase "failure to steal from a worker leads to second attempt" $ do -- {{{ 
                (broadcast_ids_list_ref,actions1) ← addAppendWorkloadStealBroadcastIdsAction bad_test_supervisor_actions
                let actions2 = ignoreAcceptWorkloadAction actions1
                SupervisorOutcome{..} ← runUnrestrictedSupervisor AllMode actions2 $ do
                    addWorker (1::Int)
                    addWorker 2
                    receiveStolenWorkload 1 Nothing
                    abortSupervisor
                supervisorTerminationReason @?= SupervisorAborted (Progress Unexplored ())
                supervisorRemainingWorkers @?= [1,2]
                readIORef broadcast_ids_list_ref >>= (@?= [[1],[1]])
             -- }}}
            ]
         -- }}}
        ,testCase "starting from previous checkpoint" $ do -- {{{
            (maybe_workload_ref,actions1) ← addAcceptOneWorkloadAction bad_test_supervisor_actions
            (broadcast_ids_list_ref,actions2) ← addAppendWorkloadStealBroadcastIdsAction actions1
            let checkpoint = ChoicePoint Unexplored Unexplored
                progress = Progress checkpoint (Sum 1)
            SupervisorOutcome{..} ← runUnrestrictedSupervisorStartingFrom AllMode progress actions2 $ do
                addWorker ()
                abortSupervisor
            supervisorTerminationReason @?= SupervisorAborted progress
            supervisorRemainingWorkers @?= [()]
            readIORef maybe_workload_ref >>= (@?= Just ((),(Workload Seq.empty checkpoint)))
            readIORef broadcast_ids_list_ref >>= (@?= [[()]])
         -- }}}
        ,testGroup "FirstMode" $ -- {{{
            [testGroup "single worker" -- {{{
                [testCase "finishes with Explored" $ do -- {{{
                    SupervisorOutcome{..} ← runUnrestrictedSupervisor FirstMode ignore_supervisor_actions $ do
                        enableSupervisorDebugMode
                        setWorkloadBufferSize 0
                        addWorker ()
                        receiveWorkerFinished () (Progress Explored Nothing)
                        error "Supervisor did not terminate"
                    supervisorTerminationReason @?= SupervisorCompleted (Nothing :: Maybe (Progress ()))
                 -- }}}
                ,testCase "finishes with result" $ do -- {{{
                    SupervisorOutcome{..} ← runUnrestrictedSupervisor FirstMode ignore_supervisor_actions $ do
                        enableSupervisorDebugMode
                        setWorkloadBufferSize 0
                        addWorker ()
                        receiveWorkerFinished () (Progress Explored (Just ()))
                        error "Supervisor did not terminate"
                    supervisorTerminationReason @?= SupervisorCompleted (Just (Progress Explored ()))
                 -- }}}
                ]
             -- }}}
            ,testGroup "two workers" -- {{{
                [testCase "both finish with Explored" $ do -- {{{
                    SupervisorOutcome{..} ← runUnrestrictedSupervisor FirstMode ignore_supervisor_actions $ do
                        enableSupervisorDebugMode
                        addWorker True
                        addWorker False
                        receiveStolenWorkload True . Just $
                            StolenWorkload
                                (ProgressUpdate
                                    Unexplored
                                    (Workload
                                        (Seq.singleton $ ChoiceStep LeftBranch)
                                        Unexplored
                                    )
                                )
                                (Workload
                                    (Seq.singleton $ ChoiceStep RightBranch)
                                    Unexplored
                                )
                        receiveWorkerFinished True (Progress (ChoicePoint Explored Unexplored) Nothing)
                        receiveWorkerFinished False (Progress (ChoicePoint Unexplored Explored) Nothing)
                        error "Supervisor did not terminate"
                    supervisorTerminationReason @?= SupervisorCompleted (Nothing :: Maybe (Progress ()))
                 -- }}}
                ,testCase "both finish with result" $ do -- {{{
                    SupervisorOutcome{..} ← runUnrestrictedSupervisor FirstMode ignore_supervisor_actions $ do
                        enableSupervisorDebugMode
                        addWorker True
                        addWorker False
                        receiveStolenWorkload True . Just $
                            StolenWorkload
                                (ProgressUpdate
                                    Unexplored
                                    (Workload
                                        (Seq.singleton $ ChoiceStep LeftBranch)
                                        Unexplored
                                    )
                                )
                                (Workload
                                    (Seq.singleton $ ChoiceStep RightBranch)
                                    Unexplored
                                )
                        receiveWorkerFinished False (Progress (ChoicePoint Explored Unexplored) (Just False))
                        receiveWorkerFinished True (Progress (ChoicePoint Unexplored Explored) (Just True))
                        error "Supervisor did not terminate"
                    supervisorTerminationReason @?= SupervisorCompleted (Just (Progress (ChoicePoint Explored Unexplored) False))
                 -- }}}
                ]
             -- }}}
            ]
         -- }}}
        ]
     -- }}}
    ,testGroup "LogicGrowsOnTrees.Parallel.Common.Worker" -- {{{
        [testGroup "forkWorkerThread" -- {{{
            [testCase "abort" $ do -- {{{
                termination_result_ivar ← IVar.new
                semaphore ← newEmptyMVar
                WorkerEnvironment{..} ← forkWorkerThread AllMode io_purity
                    (IVar.write termination_result_ivar)
                    (liftIO (takeMVar semaphore) `mplus` error "should never get here")
                    entire_workload
                    absurd
                sendAbortRequest workerPendingRequests
                putMVar semaphore ()
                termination_result ← IVar.blocking $ IVar.read termination_result_ivar
                case termination_result of
                    WorkerFinished _ → assertFailure "worker faled to abort"
                    WorkerFailed exception → assertFailure ("worker threw exception: " ++ show exception)
                    WorkerAborted → return ()
                workerInitialPath @?= Seq.empty
                (IVar.nonblocking . IVar.read) workerTerminationFlag >>= assertBool "is the termination flag set?" . isJust
             -- }}}
            ,testGroup "obtains all solutions" -- {{{
                [testProperty "with no initial path" $ \(tree :: Tree [Int]) → unsafePerformIO $ do -- {{{
                    solutions_ivar ← IVar.new
                    _ ← forkWorkerThread AllMode Pure
                            (IVar.write solutions_ivar)
                            tree
                            entire_workload
                            absurd
                    Progress checkpoint solutions ←
                        (IVar.blocking $ IVar.read solutions_ivar)
                        >>=
                        \termination_reason → case termination_reason of
                            WorkerFinished final_progress → return final_progress
                            other → error ("terminated unsuccessfully with reason " ++ show other)
                    checkpoint @?= Explored
                    solutions @?= exploreTree tree
                    return True
                 -- }}}
                ,testProperty "with an initial path" $ \(tree :: Tree [Int]) → randomPathForTree tree >>= \path → return . unsafePerformIO $ do -- {{{
                    solutions_ivar ← IVar.new
                    _ ← forkWorkerThread AllMode Pure
                            (IVar.write solutions_ivar)
                            tree
                            (Workload path Unexplored)
                            absurd
                    Progress checkpoint solutions ←
                        (IVar.blocking $ IVar.read solutions_ivar)
                        >>=
                        \termination_reason → case termination_reason of
                            WorkerFinished final_progress → return final_progress
                            other → error ("terminated unsuccessfully with reason " ++ show other)
                    checkpoint @?= checkpointFromInitialPath path Explored
                    solutions @?= (exploreTree . sendTreeDownPath path $ tree)
                    return True
                 -- }}}
                ]
             -- }}}
            ,testGroup "progress updates correctly capture current and remaining progress" $ -- {{{
                let runAnalysis tree termination_flag termination_result_ivar progress_updates_ref = do -- {{{
                        termination_result ← IVar.blocking $ IVar.read termination_result_ivar
                        remaining_solutions ← case termination_result of
                            WorkerFinished (progressResult → solutions) → return solutions
                            WorkerFailed exception → error ("worker threw exception: " ++ show exception)
                            WorkerAborted → error "worker aborted prematurely"
                        (IVar.nonblocking . IVar.read) termination_flag >>= assertBool "is the termination flag set?" . isJust
                        progress_updates ← reverse <$> readIORef progress_updates_ref
                        let correct_solutions = exploreTree tree
                            update_solutions = map (progressResult . progressUpdateProgress) progress_updates
                            all_solutions = remaining_solutions:update_solutions
                        forM_ (zip [0..] all_solutions) $ \(i,solutions_1) →
                            forM_ (zip [0..] all_solutions) $ \(j,solutions_2) →
                                unless (i == j) $
                                    assertBool "Is there an overlap between non-intersecting solutions?"
                                        (IntSet.null $ solutions_1 `IntSet.intersection` solutions_2)
                        let total_solutions = mconcat all_solutions
                        assertEqual "Are the total solutions correct?"
                            correct_solutions
                            total_solutions
                        let accumulated_update_solutions = scanl1 mappend update_solutions
                        sequence_ $
                            zipWith (\accumulated_solutions (ProgressUpdate (Progress checkpoint _) remaining_workload) → do
                                let remaining_solutions = exploreTreeWithinWorkload remaining_workload tree
                                assertBool "Is there overlap between the accumulated solutions and the remaining solutions?"
                                    (IntSet.null $ accumulated_solutions `IntSet.intersection` remaining_solutions)
                                assertEqual "Do the accumulated and remaining solutions sum to the correct solutions?"
                                    correct_solutions
                                    (accumulated_solutions `mappend` remaining_solutions)
                                assertEqual "Is the checkpoint equal to the the remaining solutions?"
                                    remaining_solutions
                                    (exploreTreeStartingFromCheckpoint checkpoint tree)
                                assertEqual "Is the inverted checkpoint equal to the the accumulated solutions?"
                                    accumulated_solutions
                                    (exploreTreeStartingFromCheckpoint (invertCheckpoint checkpoint) tree)
                             ) accumulated_update_solutions progress_updates
                        return True
                in -- }}}
                [testProperty "continuous progress update requests" $ \(UniqueTree tree) → unsafePerformIO $ do -- {{{
                    starting_flag ← IVar.new
                    termination_result_ivar ← IVar.new
                    WorkerEnvironment{..} ← forkWorkerThread AllMode io_purity
                        (IVar.write termination_result_ivar)
                        ((liftIO . IVar.blocking . IVar.read $ starting_flag) >> endowTree tree)
                        entire_workload
                        absurd
                    progress_updates_ref ← newIORef []
                    let sendMyProgressUpdateRequest = sendProgressUpdateRequest workerPendingRequests submitProgressUpdate
                        submitProgressUpdate progress_update = do
                            atomicModifyIORef progress_updates_ref ((progress_update:) &&& const ())
                            sendMyProgressUpdateRequest
                    sendMyProgressUpdateRequest
                    IVar.write starting_flag ()
                    runAnalysis tree workerTerminationFlag termination_result_ivar progress_updates_ref
                 -- }}}
                ,testProperty "progress update requests at random leaves" $ \(UniqueTree tree) → unsafePerformIO $ do -- {{{
                    termination_result_ivar ← IVar.new
                    progress_updates_ref ← newIORef []
                    rec WorkerEnvironment{..} ← forkWorkerThread AllMode io_purity
                            (IVar.write termination_result_ivar)
                            (do value ← endowTree tree
                                liftIO $ randomIO >>= flip when submitMyProgressUpdateRequest
                                return value
                            )
                            entire_workload
                            absurd
                        let submitMyProgressUpdateRequest =
                                sendProgressUpdateRequest
                                    workerPendingRequests
                                    (atomicModifyIORef progress_updates_ref . (&&& const ()) . (:))
                    runAnalysis tree workerTerminationFlag  termination_result_ivar progress_updates_ref
                 -- }}}
                ]
             -- }}}
            ,testCase "terminates successfully with null tree" $ do -- {{{
                termination_result_ivar ← IVar.new
                WorkerEnvironment{..} ←
                    forkWorkerThread AllMode Pure
                        (IVar.write termination_result_ivar)
                        (mzero :: Tree [Int])
                        entire_workload
                        absurd
                termination_result ← IVar.blocking $ IVar.read termination_result_ivar
                case termination_result of
                    WorkerFinished (progressResult → solutions) → solutions @?= mempty
                    WorkerFailed exception → assertFailure ("worker threw exception: " ++ show exception)
                    WorkerAborted → assertFailure "worker prematurely aborted"
                workerInitialPath @?= Seq.empty
                (IVar.nonblocking . IVar.read) workerTerminationFlag >>= assertBool "is the termination flag set?" . isJust
             -- }}}
            ,testGroup "work stealing correctly preserves total workload" $ -- {{{
                let runManyStealsAnalysis tree termination_flag termination_result_ivar steals_ref = do -- {{{
                        termination_result ← IVar.blocking $ IVar.read termination_result_ivar
                        (Progress _ remaining_solutions) ← case termination_result of
                            WorkerFinished final_progress → return final_progress
                            WorkerFailed exception → error ("worker threw exception: " ++ show exception)
                            WorkerAborted → error "worker aborted prematurely"
                        (IVar.nonblocking . IVar.read) termination_flag >>= assertBool "is the termination flag set?" . isJust
                        steals ← reverse <$> readIORef steals_ref
                        let correct_solutions = exploreTree tree
                            prestolen_solutions =
                                map (
                                    progressResult
                                    .
                                    progressUpdateProgress
                                    .
                                    stolenWorkloadProgressUpdate
                                ) steals
                            stolen_solutions =
                                map (
                                    flip exploreTreeWithinWorkload tree
                                    .
                                    stolenWorkload
                                ) steals
                            all_solutions = remaining_solutions:(prestolen_solutions ++ stolen_solutions)
                            total_solutions = mconcat all_solutions
                        forM_ (zip [0..] all_solutions) $ \(i,solutions_1) →
                            forM_ (zip [0..] all_solutions) $ \(j,solutions_2) →
                                unless (i == j) $
                                    assertBool "Is there overlap between non-intersecting solutions?"
                                        (IntSet.null $ solutions_1 `IntSet.intersection` solutions_2)
                        assertEqual "Do the steals together include all of the solutions?"
                            correct_solutions
                            total_solutions
                        let accumulated_prestolen_solutions = scanl1 mappend prestolen_solutions
                            accumulated_stolen_solutions = scanl1 mappend stolen_solutions
                        sequence_ $ zipWith3 (\acc_prestolen acc_stolen (StolenWorkload (ProgressUpdate (Progress checkpoint _) remaining_workload) _) → do
                            let remaining_solutions = exploreTreeWithinWorkload remaining_workload tree
                                accumulated_solutions = acc_prestolen `mappend` acc_stolen
                            assertBool "Is there overlap between the accumulated solutions and the remaining solutions?"
                                (IntSet.null $ accumulated_solutions `IntSet.intersection` remaining_solutions)
                            assertEqual "Do the accumulated and remaining solutions sum to the correct solutions?"
                                correct_solutions
                                (accumulated_solutions `mappend` remaining_solutions)
                            assertEqual "Is the checkpoint equal to the stolen plus the remaining solutions?"
                                (acc_stolen `mappend` remaining_solutions)
                                (exploreTreeStartingFromCheckpoint checkpoint tree)
                         ) accumulated_prestolen_solutions accumulated_stolen_solutions steals
                        return True
                in -- }}}
                [testProperty "single steal" $ \(UniqueTree tree :: UniqueTree) → unsafePerformIO $ do -- {{{
                    reached_position_mvar ← newEmptyMVar
                    blocking_value_ivar ← IVar.new
                    let tree_with_blocking_value =
                            mplus
                                (mplus
                                    (liftIO $ do
                                        _ ← tryPutMVar reached_position_mvar ()
                                        IVar.blocking . IVar.read $ blocking_value_ivar
                                    )
                                    (return (IntSet.singleton 101010101))
                                )
                                (endowTree tree)
                    termination_result_ivar ← IVar.new
                    WorkerEnvironment{..} ← forkWorkerThread AllMode io_purity
                        (IVar.write termination_result_ivar)
                        tree_with_blocking_value
                        entire_workload
                        absurd
                    maybe_workload_ref ← newIORef Nothing
                    takeMVar reached_position_mvar
                    sendWorkloadStealRequest workerPendingRequests $ writeIORef maybe_workload_ref
                    IVar.write blocking_value_ivar (IntSet.singleton 202020202)
                    Progress _ remaining_solutions ←
                        (IVar.blocking $ IVar.read termination_result_ivar)
                        >>=
                        \termination_result → case termination_result of
                            WorkerFinished final_progress → return final_progress
                            WorkerFailed exception → error ("worker threw exception: " ++ show exception)
                            WorkerAborted → error "worker aborted prematurely"
                    (IVar.nonblocking . IVar.read) workerTerminationFlag >>= assertBool "is the termination flag set?" . isJust
                    StolenWorkload (ProgressUpdate (Progress checkpoint prestolen_solutions) remaining_workload) stolen_workload ←
                        fmap (fromMaybe (error "stolen workload not available"))
                        $
                        readIORef maybe_workload_ref
                    assertBool "Does the checkpoint have unexplored nodes?" $ simplifyCheckpointRoot checkpoint /= Explored
                    exploreTreeTWithinWorkload remaining_workload tree_with_blocking_value >>= (remaining_solutions @?=)
                    exploreTreeTStartingFromCheckpoint (invertCheckpoint checkpoint) tree_with_blocking_value >>= (prestolen_solutions @?=)
                    correct_solutions ← exploreTreeT tree_with_blocking_value
                    stolen_solutions ← exploreTreeTWithinWorkload stolen_workload tree_with_blocking_value
                    correct_solutions @=? mconcat [prestolen_solutions,remaining_solutions,stolen_solutions]
                    assertEqual "There is no overlap between the prestolen solutions and the remaining solutions."
                        IntSet.empty
                        (prestolen_solutions `IntSet.intersection` remaining_solutions)
                    assertEqual "There is no overlap between the prestolen solutions and the stolen solutions."
                        IntSet.empty
                        (prestolen_solutions `IntSet.intersection` stolen_solutions)
                    assertEqual "There is no overlap between the stolen solutions and the remaining solutions."
                        IntSet.empty
                        (stolen_solutions `IntSet.intersection` remaining_solutions)
                    return True
                 -- }}}
                ,testProperty "continuous stealing" $ \(UniqueTree tree) → unsafePerformIO $ do -- {{{
                    starting_flag ← IVar.new
                    termination_result_ivar ← IVar.new
                    WorkerEnvironment{..} ← forkWorkerThread AllMode io_purity
                        (IVar.write termination_result_ivar)
                        ((liftIO . IVar.blocking . IVar.read $ starting_flag) >> endowTree tree)
                        entire_workload
                        absurd
                    steals_ref ← newIORef []
                    let submitMyWorkloadStealRequest = sendWorkloadStealRequest workerPendingRequests submitStolenWorkload
                        submitStolenWorkload Nothing = submitMyWorkloadStealRequest
                        submitStolenWorkload (Just steal) = do
                            atomicModifyIORef steals_ref ((steal:) &&& const ())
                            submitMyWorkloadStealRequest
                    submitMyWorkloadStealRequest
                    IVar.write starting_flag ()
                    runManyStealsAnalysis tree workerTerminationFlag termination_result_ivar steals_ref
                 -- }}}
                ,testProperty "stealing at random leaves" $ \(UniqueTree tree) → unsafePerformIO $ do -- {{{
                    termination_result_ivar ← IVar.new
                    steals_ref ← newIORef []
                    rec WorkerEnvironment{..} ← forkWorkerThread AllMode io_purity
                            (IVar.write termination_result_ivar)
                            (do value ← endowTree tree
                                liftIO $ randomIO >>= flip when submitMyWorkloadStealRequest
                                return value
                            )
                            entire_workload
                            absurd
                        let submitMyWorkloadStealRequest =
                                sendWorkloadStealRequest
                                    workerPendingRequests
                                    (maybe (return ()) $ atomicModifyIORef steals_ref . (&&& const ()) . (:))
                    runManyStealsAnalysis tree workerTerminationFlag termination_result_ivar steals_ref
                 -- }}}
                ]
             -- }}}
            ]
         -- }}}
        ,testProperty "exploreTreeUntilFirst" $ \(tree :: Tree String) → morallyDubiousIOProperty $ do -- {{{
            termination_reason ← exploreTreeGeneric FirstMode Pure tree
            case termination_reason of
                WorkerFinished maybe_final_progress → return $ (progressResult <$> maybe_final_progress) == exploreTreeUntilFirst tree
                _ → fail $ "returned " ++ show termination_reason ++ " instead of WorkerFinished"
         -- }}}
        ]
     -- }}}
    ,testGroup "LogicGrowsOnTrees.Path" -- {{{
        [testGroup "sendTreeDownPath" -- {{{
            [testCase "null path" $ (exploreTree . sendTreeDownPath Seq.empty) (return [42]) @?= [42]
            ,testCase "cache" $ do (exploreTree . sendTreeDownPath (Seq.singleton (CacheStep (encode ([42 :: Int]))))) (cache (undefined :: [Int])) @?= [42]
            ,testCase "cacheGuard" $ do (exploreTree . sendTreeDownPath (Seq.singleton (CacheStep (encode ())))) (cacheGuard False >> return [42::Int]) @?= [42]
            ,testCase "choice" $ do -- {{{
                (exploreTree . sendTreeDownPath (Seq.singleton (ChoiceStep LeftBranch))) (return [42] `mplus` undefined) @?= [42]
                (exploreTree . sendTreeDownPath (Seq.singleton (ChoiceStep RightBranch))) (undefined `mplus` return [42]) @?= [42]
             -- }}}
            ,testGroup "errors" -- {{{
                [testGroup "PastTreeIsInconsistentWithPresentTree" -- {{{
                    [testCase "cache step with choice" $ -- {{{
                        try (
                            evaluate
                            .
                            exploreTree
                            $
                            sendTreeDownPath (Seq.singleton (CacheStep undefined :: Step)) (undefined `mplus` undefined :: Tree [Int])
                        ) >>= (@?= Left PastTreeIsInconsistentWithPresentTree)
                     -- }}}
                    ,testCase "choice step with cache" $ -- {{{
                        try (
                            evaluate
                            .
                            exploreTree
                            $
                            sendTreeDownPath (Seq.singleton (ChoiceStep undefined :: Step)) (cache undefined :: Tree [Int])
                        ) >>= (@?= Left PastTreeIsInconsistentWithPresentTree)
                     -- }}}
                    ]
                 -- }}}
                ,testGroup "TreeEndedBeforeEndOfWalk" -- {{{
                    [testCase "mzero" $ -- {{{
                        try (
                            evaluate
                            .
                            exploreTree
                            $
                            sendTreeDownPath (Seq.singleton (undefined :: Step)) (mzero :: Tree [Int])
                        ) >>= (@?= Left TreeEndedBeforeEndOfWalk)
                     -- }}}
                    ,testCase "return" $ -- {{{
                        try (
                            evaluate
                            .
                            exploreTree
                            $
                            sendTreeDownPath (Seq.singleton (undefined :: Step)) (return (undefined :: [Int]))
                        ) >>= (@?= Left TreeEndedBeforeEndOfWalk)
                     -- }}}
                    ]
                 -- }}}
                ]
             -- }}}
            ]
         -- }}}
        ,testGroup "walkThroughTreeT" -- {{{
            [testCase "cache step" $ do -- {{{
                let (transformed_tree,log) =
                        runWriter . sendTreeTDownPath (Seq.singleton (CacheStep . encode $ [24 :: Int])) $ do
                            runAndCache (tell [1] >> return [42 :: Int] :: Writer [Int] [Int])
                log @?= []
                (runWriter . exploreTreeT $ transformed_tree) @?= ([24],[])
             -- }}}
            ,testCase "choice step" $ do -- {{{
                let (transformed_tree,log) =
                        runWriter . sendTreeTDownPath (Seq.singleton (ChoiceStep RightBranch)) $ do
                            lift (tell [1])
                            (lift (tell [2]) `mplus` lift (tell [3]))
                            lift (tell [4])
                            return [42]
                log @?= [1]
                (runWriter . exploreTreeT $ transformed_tree) @?= ([42],[3,4])
             -- }}}
            ]
         -- }}}
        ]
     -- }}}
    ,testGroup "LogicGrowsOnTrees.Utils.PerfectTree" -- {{{
        [Small.testProperty "trivialPerfectTree" . Small.test $ -- {{{
            (liftA2 . liftA2) (==>)
                (\arity _ → arity >= 2)
                ((liftA2 . liftA2) (==)
                    numberOfLeaves
                    ((getWordSum . exploreTree) .* trivialPerfectTree)
                )
         -- }}}
        ]
     -- }}}
    ]
-- }}}
