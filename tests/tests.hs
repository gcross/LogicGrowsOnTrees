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
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TVar
import Control.Exception
import Control.Lens (_1,_2,(%=),(<+=),use)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Operational (ProgramViewT(..),view)
import Control.Monad.STM
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

import Visitor
import Visitor.Checkpoint
import Visitor.Location
import Visitor.Parallel.Main (RunOutcome(..),TerminationReason(..))
import qualified Visitor.Parallel.BackEnd.Threads as Threads
import Visitor.Parallel.Common.ExplorationMode
import qualified Visitor.Parallel.Common.Workgroup as Workgroup
import Visitor.Path
import Visitor.Parallel.Common.Supervisor
import Visitor.Parallel.Common.Supervisor.RequestQueue
import Visitor.Utils.PerfectTree
import Visitor.Utils.WordSum
import Visitor.Workload
import qualified Visitor.Parallel.Common.Worker as Worker
import Visitor.Parallel.Common.Worker hiding (visitTree,visitTreeIO,visitTreeT,visitTreeUntilFirst,visitTreeIOUntilFirst,visitTreeTUntilFirst)
-- }}}

-- Helpers {{{

-- Instances {{{
-- Newtypes {{{
newtype UniqueVisitorT m = UniqueVisitor { unwrapUniqueVisitor :: TreeGeneratorT m IntSet }
newtype NullVisitorT m = NullVisitor { unwrapNullVisitor :: TreeGeneratorT m IntSet }
-- }}}

-- Arbitrary {{{
instance Arbitrary BranchChoice where arbitrary = elements [LeftBranch,RightBranch]

instance Arbitrary α ⇒ Arbitrary (DList α) where -- {{{
    arbitrary = DList.fromList <$> listOf arbitrary
-- }}}

instance Arbitrary UUID where -- {{{
    arbitrary = MkGen (\r _ -> fst (random r))
-- }}}

instance (Arbitrary α, Monoid α, Serialize α, Functor m, Monad m) ⇒ Arbitrary (TreeGeneratorT m α) where -- {{{
    arbitrary = fmap ($ mempty) (sized arb)
      where
        arb :: Monoid α ⇒ Int → Gen (α → TreeGeneratorT m α)
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

        null :: Gen (α → TreeGeneratorT m α)
        null = return (const mzero)

        result, cached :: Gen (TreeGeneratorT m α)
        result = fmap return arbitrary
        cached = fmap cache arbitrary

        resultPlus, cachedPlus :: Monoid α ⇒ Gen (α → TreeGeneratorT m α)
        resultPlus = (\x → flip fmap x . mappend) <$> result
        cachedPlus = (\x → flip fmap x . mappend) <$> cached
-- }}}

instance Monad m ⇒ Arbitrary (NullVisitorT m) where -- {{{
    arbitrary = (NullVisitor . ($ (const $ return ()))) <$> randomNullVisitorWithHooks
-- }}}

instance Monad m ⇒ Arbitrary (UniqueVisitorT m) where -- {{{
    arbitrary = (UniqueVisitor . ($ (const $ return ()))) <$> randomUniqueVisitorWithHooks
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
instance Show UniqueVisitor where show = show . unwrapUniqueVisitor
-- }}}
-- }}}

-- Exceptions {{{
-- TestException {{{
data TestException = TestException Int deriving (Eq,Show,Typeable)
instance Exception TestException
-- }}}
-- }}}

-- Type alises {{{
type UniqueVisitor = UniqueVisitorT Identity
type NullVisitor = NullVisitorT Identity
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

checkFoundAgainstThreshold :: Int → IntSet → Either IntSet [Int] → IO Bool -- {{{
checkFoundAgainstThreshold threshold _ (Left x) = do
    assertBool (printf "check that the unsuccessful result is small enough (%i < %i)" (IntSet.size x) threshold) $ IntSet.size x < threshold
    return True
checkFoundAgainstThreshold threshold solutions (Right x) = do
    let result = IntSet.fromList x
    assertBool "check that the result set is big enough" $ IntSet.size result >= threshold
    assertBool "check that the results are all in the full set of solutions" $ result `IntSet.isSubsetOf` solutions
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

intSetSizeFilter :: Int → IntSet → Maybe [Int] -- {{{
intSetSizeFilter threshold x
  | IntSet.size x < threshold = Nothing
  | otherwise = Just (IntSet.toList x)
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

randomCheckpointForVisitor :: Monoid α ⇒ TreeGenerator α → Gen (α,Checkpoint) -- {{{
randomCheckpointForVisitor (TreeGeneratorT visitor) = go1 visitor
  where
    go1 visitor = frequency
        [(1,return (visitTree (TreeGeneratorT visitor),Explored))
        ,(1,return (mempty,Unexplored))
        ,(3,go2 visitor)
        ]
    go2 (view → Cache (Identity (Just x)) :>>= k) =
        fmap (second $ CachePoint (encode x)) (go1 (k x))
    go2 (view → Choice (TreeGeneratorT x) (TreeGeneratorT y) :>>= k) =
        liftM2 (\(left_result,left) (right_result,right) →
            (left_result `mappend` right_result, ChoicePoint left right)
        ) (go1 (x >>= k)) (go1 (y >>= k))
    go2 visitor = elements [(visitTree (TreeGeneratorT visitor),Explored),(mempty,Unexplored)]
-- }}}

randomNullVisitorWithHooks :: ∀ m. Monad m ⇒ Gen ((Int → m ()) → TreeGeneratorT m IntSet) -- {{{
randomNullVisitorWithHooks = fmap (($ 0) . curry) . sized $ \n → evalStateT (arb1 n 0) (-1,IntSet.empty)
  where
    arb1, arb2 :: Int → Int → StateT (Int,IntSet) Gen ((Int,Int → m ()) → TreeGeneratorT m IntSet)

    arb1 n intermediate = do
        id ← _1 <+= 1
        visitor ← arb2 n intermediate
        return $ \args@(_,runHook) → lift (runHook id) >> visitor args

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
        (Int → TreeGeneratorT m Int) →
        Int →
        (Int → StateT (Int,IntSet) Gen ((Int,Int → m ()) → TreeGeneratorT m IntSet)) →
        StateT (Int,IntSet) Gen ((Int,Int → m ()) → TreeGeneratorT m IntSet)
    generateForNext construct intermediate next = do
        x ← lift arbitrary
        let new_intermediate = x `xor` intermediate
        visitor ← next new_intermediate
        return $ \(value,runHook) → do
            new_value ← construct . xor x $ value
            visitor (new_value,runHook)
    -- }}}
-- }}}

randomPathForVisitor :: TreeGenerator α → Gen Path -- {{{
randomPathForVisitor (TreeGeneratorT visitor) = go visitor
  where
    go (view → Cache (Identity (Just x)) :>>= k) = oneof
        [return Seq.empty
        ,fmap (CacheStep (encode x) <|) (go (k x))
        ]
    go (view → Choice (TreeGeneratorT x) (TreeGeneratorT y) :>>= k) = oneof
        [return Seq.empty
        ,fmap (ChoiceStep LeftBranch <|) (go (x >>= k))
        ,fmap (ChoiceStep RightBranch <|) (go (y >>= k))
        ]
    go _ = return Seq.empty
-- }}}

randomUniqueVisitorWithHooks :: ∀ m. Monad m ⇒ Gen ((Int → m ()) → TreeGeneratorT m IntSet) -- {{{
randomUniqueVisitorWithHooks = fmap (($ 0) . curry) . sized $ \n → evalStateT (arb1 n 0) (-1,IntSet.empty)
  where
    arb1, arb2 :: Int → Int → StateT (Int,IntSet) Gen ((Int,Int → m ()) → TreeGeneratorT m IntSet)

    arb1 n intermediate = do
        id ← _1 <+= 1
        visitor ← arb2 n intermediate
        return $ \args@(_,runHook) → lift (runHook id) >> visitor args

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
        (IntSet → TreeGeneratorT m IntSet) →
        Int →
        StateT (Int,IntSet) Gen ((Int,Int → m ()) → TreeGeneratorT m IntSet)
    generateUnique construct intermediate = do
        observed ← use _2
        x ← lift (arbitrary `suchThat` (flip IntSet.notMember observed . (xor intermediate)))
        let final_value = x `xor` intermediate
        _2 %= IntSet.insert final_value
        return $ construct . IntSet.singleton . xor x . fst
    -- }}}

    generateForNext :: -- {{{
        Monad m ⇒
        (Int → TreeGeneratorT m Int) →
        Int →
        (Int → StateT (Int,IntSet) Gen ((Int,Int → m ()) → TreeGeneratorT m IntSet)) →
        StateT (Int,IntSet) Gen ((Int,Int → m ()) → TreeGeneratorT m IntSet)
    generateForNext construct intermediate next = do
        x ← lift arbitrary
        let new_intermediate = x `xor` intermediate
        visitor ← next new_intermediate
        return $ \(value,runHook) → do
            new_value ← construct . xor x $ value
            visitor (new_value,runHook)
    -- }}}
-- }}}

randomVisitorWithoutCache :: Arbitrary α ⇒ Gen (TreeGenerator α) -- {{{
randomVisitorWithoutCache = sized arb
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
endless_visitor = endless_visitor `mplus` endless_visitor
-- }}}

-- }}}

main = do
    -- updateGlobalLogger rootLoggerName (setLevel DEBUG)
    defaultMain tests

tests = -- {{{
    [testGroup "test helpers" $ -- {{{
        [testProperty "UniqueVisitor has unique results" $ \(UniqueVisitor visitor) → -- {{{
            let results = visitTree (fmap (:[]) visitor )
            in length results == IntSet.size (mconcat results)
         -- }}}
        ]
     -- }}}
    ,testGroup "Visitor" -- {{{
        [testGroup "Eq instance" -- {{{
            [testProperty "self" $ \(v :: TreeGenerator [()]) → v == v
            ]
         -- }}}
        ,testProperty "allFrom" $ \(x :: [Int]) → x == allFrom x
        ,testProperty "allFromBalanced" $ \(x :: [Int]) → x == allFromBalanced x
        ,testProperty "allFromBalancedBottomUp" $ \(x :: [Int]) → ((==) `on` sort) x (allFromBalancedBottomUp x)
        ,testProperty "between" $ do -- {{{
            x ← choose ( 0,100) :: Gen Int
            y ← choose (50,100)
            return $ between x y == [x..y]
         -- }}}
        ,testProperty "msumBalanced" $ \(x :: [Int]) → x == msumBalanced (map return x)
        ,testProperty "msumBalancedBottomUp" $ \(x :: [UUID]) → ((==) `on` sort) x (msumBalancedBottomUp (map return x))
        ,testGroup "visitTree" -- {{{
            [testCase "return" $ visitTree (return [()]) @?= [()]
            ,testCase "mzero" $ visitTree (mzero :: TreeGenerator [()]) @?= []
            ,testCase "mplus" $ visitTree (return [1::Int] `mplus` return [2]) @?= [1,2]
            ,testCase "cache" $ visitTree (cache [42]) @?= [42::Int]
            ,testGroup "cacheMaybe" -- {{{
                [testCase "Nothing" $ visitTree (cacheMaybe (Nothing :: Maybe [()])) @?= []
                ,testCase "Just" $ visitTree (cacheMaybe (Just [42])) @?= [42::Int]
                ]
             -- }}}
            ,testGroup "cacheGuard" -- {{{
                [testCase "True" $ visitTree (cacheGuard False >> return [()]) @?= []
                ,testCase "False" $ visitTree (cacheGuard True >> return [()]) @?= [()]
                ]
             -- }}}
            ]
         -- }}}
        ,testGroup "visitTreeT" -- {{{
            [testCase "Writer" $ -- {{{
                (runWriter . visitTreeT $ do
                    cache [1 :: Int] >>= lift . tell
                    (lift (tell [2]) `mplus` lift (tell [3]))
                    return [42::Int]
                ) @?= ([42,42],[1,2,3])
             -- }}}
            ]
         -- }}}
        ,testGroup "visitTreeTAndIgnoreResults" -- {{{
            [testCase "Writer" $ -- {{{
                (runWriter . visitTreeTAndIgnoreResults $ do
                    cache [1 :: Int] >>= lift . tell
                    (lift (tell [2]) `mplus` lift (tell [3]))
                    return [42::Int]
                ) @?= ((),[1,2,3])
             -- }}}
            ]
         -- }}}
        ,testGroup "visitTreeUntilFirst" -- {{{
            [testCase "return" $ visitTreeUntilFirst (return 42) @=? (Just 42 :: Maybe Int)
            ,testCase "null" $ visitTreeUntilFirst mzero @=? (Nothing :: Maybe Int)
            ,testProperty "compared to visitTree" $ \(visitor :: TreeGenerator String) →
                visitTreeUntilFirst visitor
                ==
                case visitTree (fmap (:[]) visitor) of
                    [] → Nothing
                    (x:_) → Just x
            ]
         -- }}}
        ,testGroup "visitTreeTUntilFirst" -- {{{
            [testCase "return" $ runIdentity (visitTreeTUntilFirst (return 42)) @=? (Just 42 :: Maybe Int)
            ,testCase "null" $ runIdentity(visitTreeTUntilFirst mzero) @=? (Nothing :: Maybe Int)
            ,testProperty "compared to visitTreeT" $ \(visitor :: TreeGeneratorT Identity String) →
                runIdentity (visitTreeTUntilFirst visitor)
                ==
                case runIdentity (visitTreeT (fmap (:[]) visitor)) of
                    [] → Nothing
                    (x:_) → Just x
            ]
         -- }}}
        ,testGroup "visitTreeUntilFound" -- {{{
            [testProperty "compared to visitTree" $ do
                UniqueVisitor visitor ← arbitrary
                let solutions = visitTree visitor
                threshold ← (+1) <$> choose (0,2*IntSet.size solutions)
                return . unsafePerformIO . checkFoundAgainstThreshold threshold solutions $
                    visitTreeUntilFound (intSetSizeFilter threshold) visitor
            ]
         -- }}}
        ,testGroup "visitTreeTUntilFound" -- {{{
            [testProperty "compared to visitTreeT" $ do
                UniqueVisitor visitor ← arbitrary
                let solutions = runIdentity (visitTreeT visitor)
                threshold ← (+1) <$> choose (0,2*IntSet.size solutions)
                let f x | IntSet.size x < threshold = Nothing
                        | otherwise = Just (IntSet.toList x)
                    found_solutions = runIdentity (visitTreeTUntilFound f visitor)
                return $ case found_solutions of
                    Left x → IntSet.size x < threshold
                    Right x →
                        let result = IntSet.fromList x
                        in IntSet.size result >= threshold &&
                            result `IntSet.isSubsetOf` solutions
            ]
         -- }}}
        ]
     -- }}}
    ,testGroup "Visitor.Checkpoint" -- {{{
        [testGroup "contextFromCheckpoint" -- {{{
            [testProperty "cache" $ \(checkpoint :: Checkpoint) (i :: Int) → -- {{{
                checkpointFromContext (Seq.singleton (CacheContextStep (encode i))) checkpoint
                ==
                (simplifyCheckpointRoot $ CachePoint (encode i) checkpoint)
             -- }}}
            ,testProperty "left branch" $ \(inner_checkpoint :: Checkpoint) (other_visitor :: TreeGenerator [()]) (other_checkpoint :: Checkpoint) → -- {{{
                (checkpointFromContext (Seq.singleton (LeftBranchContextStep other_checkpoint other_visitor)) inner_checkpoint)
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
        ,testProperty "invertCheckpoint" $ \(visitor :: TreeGenerator (Set UUID)) → -- {{{
            randomCheckpointForVisitor visitor >>= \(partial_result,checkpoint) → return $
                partial_result == visitTreeStartingFromCheckpoint (invertCheckpoint checkpoint) visitor
         -- }}}
        ,testGroup "Monoid instance" -- {{{
            [testProperty "product results in intersection of solutions" $ \(UniqueVisitor visitor) → do -- {{{
                (_,checkpoint1) ← randomCheckpointForVisitor visitor
                (_,checkpoint2) ← randomCheckpointForVisitor visitor
                let checkpoint3 = checkpoint1 `mappend` checkpoint2
                    solutions1 = visitTreeStartingFromCheckpoint checkpoint1 visitor
                    solutions2 = visitTreeStartingFromCheckpoint checkpoint2 visitor
                    solutions3 = visitTreeStartingFromCheckpoint checkpoint3 visitor
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
            UniqueVisitor visitor ← arbitrary
            (partial_result,checkpoint) ← randomCheckpointForVisitor visitor
            let go state@VisitorTState{..} current_result =
                    visitTreeStartingFromCheckpoint
                        (invertCheckpoint (checkpointFromVisitorState state))
                        visitor
                    ==
                    current_result
                    &&
                    case stepThroughTreeStartingFromCheckpoint state of
                        (Just result,Nothing) → visitTree visitor == current_result <> result
                        (Nothing,Nothing) → visitTree visitor == current_result
                        (Just result,Just new_state) → go new_state (current_result <> result)
                        (Nothing,Just new_state) → go new_state current_result
            return $ go (initialVisitorState checkpoint visitor) partial_result
         -- }}}
        ,testProperty "visitTreeStartingFromCheckpoint" $ \(UniqueVisitor visitor) → -- {{{
            randomCheckpointForVisitor visitor >>= \(partial_result,checkpoint) → return $
                visitTree visitor ==
                    mappend partial_result (visitTreeStartingFromCheckpoint checkpoint visitor)
         -- }}}
        ,testProperty "visitTreeUntilFirstStartingFromCheckpoint" $ \(UniqueVisitor visitor) → -- {{{
            randomCheckpointForVisitor visitor >>= \(_,checkpoint) → return $
                let all_results = visitTreeStartingFromCheckpoint checkpoint visitor
                    maybe_first_result = visitTreeUntilFirstStartingFromCheckpoint checkpoint visitor
                in case maybe_first_result of
                    Nothing → IntSet.null all_results
                    Just result → IntSet.size result == 1 && IntSet.member (IntSet.findMin result) all_results
         -- }}}
        ,testProperty "visitTreeTUntilFirstStartingFromCheckpoint" $ \(UniqueVisitor visitor) → -- {{{
            randomCheckpointForVisitor visitor >>= \(_,checkpoint) → return $
                let all_results = visitTreeStartingFromCheckpoint checkpoint visitor
                    maybe_first_result = runIdentity $ visitTreeTUntilFirstStartingFromCheckpoint checkpoint visitor
                in case maybe_first_result of
                    Nothing → IntSet.null all_results
                    Just result → IntSet.size result == 1 && IntSet.member (IntSet.findMin result) all_results
         -- }}}
        ,testProperty "visitTreeUntilFoundStartingFromCheckpoint" $ do -- {{{
            UniqueVisitor visitor ← arbitrary
            (_,checkpoint) ← randomCheckpointForVisitor visitor
            let solutions = visitTreeStartingFromCheckpoint checkpoint visitor
            threshold ← (+1) <$> choose (0,2*IntSet.size solutions)
            return . unsafePerformIO . checkFoundAgainstThreshold threshold solutions $
                visitTreeUntilFoundStartingFromCheckpoint (intSetSizeFilter threshold) checkpoint visitor
         -- }}}
        ,testProperty "visitTreeTUntilFoundStartingFromCheckpoint" $ do -- {{{
            UniqueVisitor visitor ← arbitrary
            (_,checkpoint) ← randomCheckpointForVisitor visitor
            let solutions = visitTreeStartingFromCheckpoint checkpoint visitor
            threshold ← (+1) <$> choose (0,2*IntSet.size solutions)
            return . unsafePerformIO . checkFoundAgainstThreshold threshold solutions . runIdentity $
                visitTreeTUntilFoundStartingFromCheckpoint (intSetSizeFilter threshold) checkpoint visitor
          -- }}}
        ]
     -- }}}
    ,testGroup "Visitor.Location" -- {{{
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
        ,testGroup "visitTreeWithLocations" -- {{{
            [testProperty "same result as visitTree" $ \(visitor :: TreeGenerator [()]) →
                 visitTree ((:[]) <$> visitor) == (solutionResult <$> visitTreeWithLocations visitor)
            ]
         -- }}}
        ,testGroup "sendTreeGeneratorDownLocation" -- {{{
            [testProperty "same result as walking down path" $ do -- {{{
                visitor :: TreeGenerator Int ← randomVisitorWithoutCache
                path ← randomPathForVisitor visitor
                let label = labelFromPath path
                return $
                    sendTreeGeneratorDownPath path visitor
                    ==
                    sendTreeGeneratorDownLocation label visitor
             -- }}}
            ]
         -- }}}
        ,testProperty "visitLocatableTree" $ -- {{{
            let gen _ 0 = return mzero
                gen label 1 = return (All . (== label) <$> getLocation)
                gen label n = do
                    left_size ← choose (0,n)
                    let right_size = n-left_size
                    left ← gen (leftBranchOf label) left_size
                    right ← gen (rightBranchOf label) right_size
                    return $ left `mplus` right
            in getAll . visitLocatableTree <$> sized (gen rootLocation)
         -- }}}
        ]
     -- }}}
    ,testGroup "Visitor.Parallel.BackEnd.Threads" $ -- {{{
        [testGroup "FirstMode" -- {{{
            [testCase "two threads, one blocked" $ do -- {{{
                RunOutcome _ termination_reason ←
                    Threads.visitTreeIOUntilFirst
                        (liftIO (threadDelay 1) >> endless_visitor
                         `mplus`
                         return ()
                        )
                        (void . Workgroup.changeNumberOfWorkers . const . return $ 2)
                termination_reason @?= Completed (Just (Progress (ChoicePoint Unexplored Explored) ()))
             -- }}}
            ]
         -- }}}
        ,testGroup "FoundModeUsingPull" -- {{{
            [testCase "many threads with combined final result but none finish" $ do -- {{{
                RunOutcome _ termination_reason ←
                    Threads.visitTreeIOUntilFoundUsingPull
                        (\xs → if length xs == 2 then Just (sum xs) else Nothing)
                        ((return [1] `mplus` endless_visitor) `mplus` (return [2] `mplus` endless_visitor))
                        (void . Workgroup.changeNumberOfWorkers . const . return $ 4)
                case termination_reason of
                    Completed (Right (Progress _ result)) → result @?= (3,[])
                    _ → fail $ "got incorrect result: " ++ show termination_reason
             -- }}}
            ]
         -- }}}
        ,testGroup "FoundModeUsingPush" -- {{{
            [testCase "two threads with combined final result but none finish" $ do -- {{{
                RunOutcome _ termination_reason ←
                    Threads.visitTreeIOUntilFoundUsingPush
                        (\xs → if length xs == 2 then Just (sum xs) else Nothing)
                        ((return [1] `mplus` endless_visitor) `mplus` (return [2] `mplus` endless_visitor))
                        (void . Workgroup.changeNumberOfWorkers . const . return $ 2)
                case termination_reason of
                    Completed (Right (Progress _ result)) → result @?= 3
                    _ → fail $ "got incorrect result: " ++ show termination_reason
             -- }}}
            ]
         -- }}}
        ,plusTestOptions (mempty {topt_maximum_generated_tests = Just 10}) $ testGroup "stress tests" $ -- {{{
            let extractResult (RunOutcome _ termination_reason) = -- {{{
                    case termination_reason of
                        Aborted _ → error "prematurely aborted"
                        Completed result → return result
                        Failure message → error message
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
                let runTest generateNoise = randomUniqueVisitorWithHooks >>= \constructVisitor → morallyDubiousIOProperty $ do
                        cleared_flags_mvar ← newMVar mempty
                        request_queue ← newChan
                        progresses_ref ← newIORef []
                        result ←
                            (Threads.visitTreeIO
                                (insertHooks cleared_flags_mvar request_queue constructVisitor)
                                (respondToRequests request_queue generateNoise progresses_ref)
                            ) >>= extractResult
                        let visitor = constructVisitor (const $ return ())
                        correct_result ← visitTreeT visitor
                        result @?= correct_result
                        (remdups <$> readIORef progresses_ref) >>= mapM_ (\(Progress checkpoint result) → do
                            visitTreeTStartingFromCheckpoint (invertCheckpoint checkpoint) visitor >>= (@?= result)
                            visitTreeTStartingFromCheckpoint checkpoint visitor >>= (@?= correct_result ) . mappend result
                         )
                        return True
              in
              [testProperty "one thread" . runTest $ oneThreadNoise
              ,testProperty "two threads" . runTest $ twoThreadsNoise
              ,testProperty "many threads" . runTest $ manyThreadsNoise
              ]
             -- }}}
            ,testGroup "FirstMode" $ -- {{{
                let runTest generator generateNoise = generator >>= \constructVisitor → morallyDubiousIOProperty $ do
                        cleared_flags_mvar ← newMVar mempty
                        request_queue ← newChan
                        progresses_ref ← newIORef []
                        maybe_result ←
                            (Threads.visitTreeIOUntilFirst
                                (insertHooks cleared_flags_mvar request_queue constructVisitor)
                                (respondToRequests request_queue generateNoise progresses_ref)
                            ) >>= extractResult
                        let visitor = constructVisitor (const $ return ())
                        correct_results ← visitTreeT visitor
                        case maybe_result of
                            Nothing → assertBool "solutions were missed" (IntSet.null correct_results)
                            Just (Progress checkpoint result) → do
                                IntSet.size result @?= 1
                                assertBool "solution was not valid" $ result `IntSet.isSubsetOf` correct_results
                                visitTreeTStartingFromCheckpoint (invertCheckpoint checkpoint) visitor >>= (@=? result)
                                visitTreeTStartingFromCheckpoint checkpoint visitor >>= (@=? IntSet.difference correct_results result)
                        (remdups <$> readIORef progresses_ref) >>= mapM_ (\checkpoint → do
                            visitTreeTUntilFirstStartingFromCheckpoint (invertCheckpoint checkpoint) visitor >>= (@?= Nothing)
                         )
                        return True
                    testGroupUsingGenerator name generator = testGroup name $
                        [testProperty "one thread" . runTest generator $ oneThreadNoise
                        ,testProperty "two threads" . runTest generator $ twoThreadsNoise
                        ,testProperty "many threads" . runTest generator $ manyThreadsNoise
                        ]
                in [testGroupUsingGenerator "with solutions" randomUniqueVisitorWithHooks
                   ,testGroupUsingGenerator "without solutions" randomNullVisitorWithHooks
                   ]
             -- }}}
            ,testGroup "FoundModeUsingPull" $ -- {{{
                let runTest generator generateNoise = generator >>= \constructVisitor → morallyDubiousIOProperty $ do
                        let visitor = constructVisitor (const $ return ())
                        correct_results ← visitTreeT visitor
                        number_of_results_to_find ← randomRIO (1,2*IntSet.size correct_results)
                        cleared_flags_mvar ← newMVar mempty
                        request_queue ← newChan
                        progresses_ref ← newIORef []
                        result ←
                            (Threads.visitTreeIOUntilFoundUsingPull
                                (\result → if IntSet.size result >= number_of_results_to_find
                                    then Just $ IntSet.toList result
                                    else Nothing
                                )
                                (insertHooks cleared_flags_mvar request_queue constructVisitor)
                                (respondToRequests request_queue generateNoise progresses_ref)
                            ) >>= extractResult
                        case result of
                            Left incomplete_result → do
                                assertBool "result is not smaller than desired" $ IntSet.size incomplete_result < number_of_results_to_find
                                assertEqual "incomplete result matches correct result" incomplete_result correct_results
                            Right (Progress checkpoint (final_result_as_list,leftover_result)) → do
                                let final_result = IntSet.fromList final_result_as_list
                                assertBool "result is at least as large as desired" $ IntSet.size final_result >= number_of_results_to_find
                                assertBool "final result was not valid" $ final_result `IntSet.isSubsetOf` correct_results
                                assertBool "leftover result was not valid" $ leftover_result `IntSet.isSubsetOf` correct_results
                                assertBool "final and leftover results overlap" . IntSet.null $ IntSet.intersection final_result leftover_result
                                let all_results = final_result `mappend` leftover_result
                                visitTreeTStartingFromCheckpoint (invertCheckpoint checkpoint) visitor
                                    >>= assertEqual "both returned results together do not match all results covered by the checkpoint" all_results
                                visitTreeTStartingFromCheckpoint checkpoint visitor
                                    >>= assertEqual "all results minus return results do not match remaining results" (IntSet.difference correct_results all_results)
                        (remdups <$> readIORef progresses_ref) >>= mapM_ (\(Progress checkpoint result) → do
                            visitTreeTStartingFromCheckpoint (invertCheckpoint checkpoint) visitor >>= (@?= result)
                            visitTreeTStartingFromCheckpoint checkpoint visitor >>= (@?= correct_results) . mappend result
                         )
                        return True
                    testGroupUsingGenerator name generator = testGroup name $
                        [testProperty "one thread" . runTest generator $ oneThreadNoise
                        ,testProperty "two threads" . runTest generator $ twoThreadsNoise
                        ,testProperty "many threads" . runTest generator $ manyThreadsNoise
                        ]
                in [testGroupUsingGenerator "with solutions" randomUniqueVisitorWithHooks
                   ,testGroupUsingGenerator "without solutions" randomNullVisitorWithHooks
                   ]
             -- }}}
            ,testGroup "FoundModeUsingPush" $ -- {{{
                let runTest generator generateNoise = generator >>= \constructVisitor → morallyDubiousIOProperty $ do
                        let visitor = constructVisitor (const $ return ())
                        correct_results ← visitTreeT visitor
                        number_of_results_to_find ← randomRIO (1,2*IntSet.size correct_results)
                        cleared_flags_mvar ← newMVar mempty
                        request_queue ← newChan
                        progresses_ref ← newIORef []
                        result ←
                            (Threads.visitTreeIOUntilFoundUsingPush
                                (\result → if IntSet.size result >= number_of_results_to_find
                                    then Just $ IntSet.toList result
                                    else Nothing
                                )
                                (insertHooks cleared_flags_mvar request_queue constructVisitor)
                                (respondToRequests request_queue generateNoise progresses_ref)
                            ) >>= extractResult
                        case result of
                            Left incomplete_result → do
                                assertBool "result is not smaller than desired" $ IntSet.size incomplete_result < number_of_results_to_find
                                assertEqual "incomplete result matches correct result" incomplete_result correct_results
                            Right (Progress checkpoint final_result_as_list) → do
                                let final_result = IntSet.fromList final_result_as_list
                                assertBool "result is at least as large as desired" $ IntSet.size final_result >= number_of_results_to_find
                                assertBool "final result was not valid" $ final_result `IntSet.isSubsetOf` correct_results
                                visitTreeTStartingFromCheckpoint (invertCheckpoint checkpoint) visitor
                                    >>= assertEqual "both returned results together do not match all results covered by the checkpoint" final_result
                                visitTreeTStartingFromCheckpoint checkpoint visitor
                                    >>= assertEqual "all results minus return results do not match remaining results" (IntSet.difference correct_results final_result)
                        (remdups <$> readIORef progresses_ref) >>= mapM_ (\(Progress checkpoint result) → do
                            visitTreeTStartingFromCheckpoint (invertCheckpoint checkpoint) visitor >>= (@?= result)
                            visitTreeTStartingFromCheckpoint checkpoint visitor >>= (@?= correct_results) . mappend result
                         )
                        return True
                    testGroupUsingGenerator name generator = testGroup name $
                        [testProperty "one thread" . runTest generator $ oneThreadNoise
                        ,testProperty "two threads" . runTest generator $ twoThreadsNoise
                        ,testProperty "many threads" . runTest generator $ manyThreadsNoise
                        ]
                in [testGroupUsingGenerator "with solutions" randomUniqueVisitorWithHooks
                   ,testGroupUsingGenerator "without solutions" randomNullVisitorWithHooks
                   ]
             -- }}}
            ]
         -- }}}
        ]
     -- }}}
    ,testGroup "Visitor.Parallel.Common.Supervisor" -- {{{
        [testCase "immediately abort" $ do -- {{{
            SupervisorOutcome{..} ← runSupervisor AllMode bad_test_supervisor_actions (UnrestrictedProgram abortSupervisor)
            supervisorTerminationReason @?= SupervisorAborted (Progress Unexplored ())
            supervisorRemainingWorkers @?= ([] :: [Int])
         -- }}}
        ,testCase "failure" $ do -- {{{
            SupervisorOutcome{..} ← runUnrestrictedSupervisor AllMode bad_test_supervisor_actions (receiveWorkerFailure () "FAIL" :: ∀ α. SupervisorMonad (AllMode ()) () IO α)
            supervisorTerminationReason @?= SupervisorFailure () "FAIL"
            supervisorRemainingWorkers @?= []
         -- }}}
        ,testGroup "adding and removing workers" -- {{{
            [testGroup "without workload buffer" -- {{{
                [testCase "add one worker then abort" $ do -- {{{
                    (maybe_workload_ref,actions) ← addAcceptOneWorkloadAction bad_test_supervisor_actions
                    SupervisorOutcome{..} ← runUnrestrictedSupervisor AllMode actions $ do
                        enableSupervisorDebugMode
                        killWorkloadBuffer
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
                        killWorkloadBuffer
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
                        killWorkloadBuffer
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
                        killWorkloadBuffer
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
                            killWorkloadBuffer
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
                    killWorkloadBuffer
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
                        killWorkloadBuffer
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
                    killWorkloadBuffer
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
                    killWorkloadBuffer
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
                        killWorkloadBuffer
                        addWorker ()
                        receiveWorkerFinished () (Progress Explored Nothing)
                        error "Supervisor did not terminate"
                    supervisorTerminationReason @?= SupervisorCompleted (Nothing :: Maybe (Progress ()))
                 -- }}}
                ,testCase "finishes with result" $ do -- {{{
                    SupervisorOutcome{..} ← runUnrestrictedSupervisor FirstMode ignore_supervisor_actions $ do
                        enableSupervisorDebugMode
                        killWorkloadBuffer
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
    ,testGroup "Visitor.Parallel.Common.Worker" -- {{{
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
                [testProperty "with no initial path" $ \(visitor :: TreeGenerator [Int]) → unsafePerformIO $ do -- {{{
                    solutions_ivar ← IVar.new
                    _ ← forkWorkerThread AllMode Pure
                            (IVar.write solutions_ivar)
                            visitor
                            entire_workload
                            absurd
                    Progress checkpoint solutions ←
                        (IVar.blocking $ IVar.read solutions_ivar)
                        >>=
                        \termination_reason → case termination_reason of
                            WorkerFinished final_progress → return final_progress
                            other → error ("terminated unsuccessfully with reason " ++ show other)
                    checkpoint @?= Explored
                    solutions @?= visitTree visitor
                    return True
                 -- }}}
                ,testProperty "with an initial path" $ \(visitor :: TreeGenerator [Int]) → randomPathForVisitor visitor >>= \path → return . unsafePerformIO $ do -- {{{
                    solutions_ivar ← IVar.new
                    _ ← forkWorkerThread AllMode Pure
                            (IVar.write solutions_ivar)
                            visitor
                            (Workload path Unexplored)
                            absurd
                    Progress checkpoint solutions ←
                        (IVar.blocking $ IVar.read solutions_ivar)
                        >>=
                        \termination_reason → case termination_reason of
                            WorkerFinished final_progress → return final_progress
                            other → error ("terminated unsuccessfully with reason " ++ show other)
                    checkpoint @?= checkpointFromInitialPath path Explored
                    solutions @?= (visitTree . sendTreeGeneratorDownPath path $ visitor)
                    return True
                 -- }}}
                ]
             -- }}}
            ,testGroup "progress updates correctly capture current and remaining progress" $ -- {{{
                let runAnalysis visitor termination_flag termination_result_ivar progress_updates_ref = do -- {{{
                        termination_result ← IVar.blocking $ IVar.read termination_result_ivar
                        remaining_solutions ← case termination_result of
                            WorkerFinished (progressResult → solutions) → return solutions
                            WorkerFailed exception → error ("worker threw exception: " ++ show exception)
                            WorkerAborted → error "worker aborted prematurely"
                        (IVar.nonblocking . IVar.read) termination_flag >>= assertBool "is the termination flag set?" . isJust
                        progress_updates ← reverse <$> readIORef progress_updates_ref
                        let correct_solutions = visitTree visitor
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
                                let remaining_solutions = visitTreeWithinWorkload remaining_workload visitor
                                assertBool "Is there overlap between the accumulated solutions and the remaining solutions?"
                                    (IntSet.null $ accumulated_solutions `IntSet.intersection` remaining_solutions)
                                assertEqual "Do the accumulated and remaining solutions sum to the correct solutions?"
                                    correct_solutions
                                    (accumulated_solutions `mappend` remaining_solutions)
                                assertEqual "Is the checkpoint equal to the the remaining solutions?"
                                    remaining_solutions
                                    (visitTreeStartingFromCheckpoint checkpoint visitor)
                                assertEqual "Is the inverted checkpoint equal to the the accumulated solutions?"
                                    accumulated_solutions
                                    (visitTreeStartingFromCheckpoint (invertCheckpoint checkpoint) visitor)
                             ) accumulated_update_solutions progress_updates
                        return True
                in -- }}}
                [testProperty "continuous progress update requests" $ \(UniqueVisitor visitor) → unsafePerformIO $ do -- {{{
                    starting_flag ← IVar.new
                    termination_result_ivar ← IVar.new
                    WorkerEnvironment{..} ← forkWorkerThread AllMode io_purity
                        (IVar.write termination_result_ivar)
                        ((liftIO . IVar.blocking . IVar.read $ starting_flag) >> endowTreeGenerator visitor)
                        entire_workload
                        absurd
                    progress_updates_ref ← newIORef []
                    let sendMyProgressUpdateRequest = sendProgressUpdateRequest workerPendingRequests submitProgressUpdate
                        submitProgressUpdate progress_update = do
                            atomicModifyIORef progress_updates_ref ((progress_update:) &&& const ())
                            sendMyProgressUpdateRequest
                    sendMyProgressUpdateRequest
                    IVar.write starting_flag ()
                    runAnalysis visitor workerTerminationFlag termination_result_ivar progress_updates_ref
                 -- }}}
                ,testProperty "progress update requests at random leaves" $ \(UniqueVisitor visitor) → unsafePerformIO $ do -- {{{
                    termination_result_ivar ← IVar.new
                    progress_updates_ref ← newIORef []
                    rec WorkerEnvironment{..} ← forkWorkerThread AllMode io_purity
                            (IVar.write termination_result_ivar)
                            (do value ← endowTreeGenerator visitor
                                liftIO $ randomIO >>= flip when submitMyProgressUpdateRequest
                                return value
                            )
                            entire_workload
                            absurd
                        let submitMyProgressUpdateRequest =
                                sendProgressUpdateRequest
                                    workerPendingRequests
                                    (atomicModifyIORef progress_updates_ref . (&&& const ()) . (:))
                    runAnalysis visitor workerTerminationFlag  termination_result_ivar progress_updates_ref
                 -- }}}
                ]
             -- }}}
            ,testCase "terminates successfully with null visitor" $ do -- {{{
                termination_result_ivar ← IVar.new
                WorkerEnvironment{..} ←
                    forkWorkerThread AllMode Pure
                        (IVar.write termination_result_ivar)
                        (mzero :: TreeGenerator [Int])
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
                let runManyStealsAnalysis visitor termination_flag termination_result_ivar steals_ref = do -- {{{
                        termination_result ← IVar.blocking $ IVar.read termination_result_ivar
                        (Progress _ remaining_solutions) ← case termination_result of
                            WorkerFinished final_progress → return final_progress
                            WorkerFailed exception → error ("worker threw exception: " ++ show exception)
                            WorkerAborted → error "worker aborted prematurely"
                        (IVar.nonblocking . IVar.read) termination_flag >>= assertBool "is the termination flag set?" . isJust
                        steals ← reverse <$> readIORef steals_ref
                        let correct_solutions = visitTree visitor
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
                                    flip visitTreeWithinWorkload visitor
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
                            let remaining_solutions = visitTreeWithinWorkload remaining_workload visitor
                                accumulated_solutions = acc_prestolen `mappend` acc_stolen
                            assertBool "Is there overlap between the accumulated solutions and the remaining solutions?"
                                (IntSet.null $ accumulated_solutions `IntSet.intersection` remaining_solutions)
                            assertEqual "Do the accumulated and remaining solutions sum to the correct solutions?"
                                correct_solutions
                                (accumulated_solutions `mappend` remaining_solutions)
                            assertEqual "Is the checkpoint equal to the stolen plus the remaining solutions?"
                                (acc_stolen `mappend` remaining_solutions)
                                (visitTreeStartingFromCheckpoint checkpoint visitor)
                         ) accumulated_prestolen_solutions accumulated_stolen_solutions steals
                        return True
                in -- }}}
                [testProperty "single steal" $ \(UniqueVisitor visitor :: UniqueVisitor) → unsafePerformIO $ do -- {{{
                    reached_position_mvar ← newEmptyMVar
                    blocking_value_ivar ← IVar.new
                    let visitor_with_blocking_value =
                            mplus
                                (mplus
                                    (liftIO $ do
                                        _ ← tryPutMVar reached_position_mvar ()
                                        IVar.blocking . IVar.read $ blocking_value_ivar
                                    )
                                    (return (IntSet.singleton 101010101))
                                )
                                (endowTreeGenerator visitor)
                    termination_result_ivar ← IVar.new
                    WorkerEnvironment{..} ← forkWorkerThread AllMode io_purity
                        (IVar.write termination_result_ivar)
                        visitor_with_blocking_value
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
                    visitTreeTWithinWorkload remaining_workload visitor_with_blocking_value >>= (remaining_solutions @?=)
                    visitTreeTStartingFromCheckpoint (invertCheckpoint checkpoint) visitor_with_blocking_value >>= (prestolen_solutions @?=)
                    correct_solutions ← visitTreeT visitor_with_blocking_value
                    stolen_solutions ← visitTreeTWithinWorkload stolen_workload visitor_with_blocking_value
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
                ,testProperty "continuous stealing" $ \(UniqueVisitor visitor) → unsafePerformIO $ do -- {{{
                    starting_flag ← IVar.new
                    termination_result_ivar ← IVar.new
                    WorkerEnvironment{..} ← forkWorkerThread AllMode io_purity
                        (IVar.write termination_result_ivar)
                        ((liftIO . IVar.blocking . IVar.read $ starting_flag) >> endowTreeGenerator visitor)
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
                    runManyStealsAnalysis visitor workerTerminationFlag termination_result_ivar steals_ref
                 -- }}}
                ,testProperty "stealing at random leaves" $ \(UniqueVisitor visitor) → unsafePerformIO $ do -- {{{
                    termination_result_ivar ← IVar.new
                    steals_ref ← newIORef []
                    rec WorkerEnvironment{..} ← forkWorkerThread AllMode io_purity
                            (IVar.write termination_result_ivar)
                            (do value ← endowTreeGenerator visitor
                                liftIO $ randomIO >>= flip when submitMyWorkloadStealRequest
                                return value
                            )
                            entire_workload
                            absurd
                        let submitMyWorkloadStealRequest =
                                sendWorkloadStealRequest
                                    workerPendingRequests
                                    (maybe (return ()) $ atomicModifyIORef steals_ref . (&&& const ()) . (:))
                    runManyStealsAnalysis visitor workerTerminationFlag termination_result_ivar steals_ref
                 -- }}}
                ]
             -- }}}
            ]
         -- }}}
        ,testProperty "visitTreeUntilFirst" $ \(visitor :: TreeGenerator String) → morallyDubiousIOProperty $ do -- {{{
            termination_reason ← Worker.visitTreeGeneric FirstMode Pure visitor
            case termination_reason of
                WorkerFinished maybe_final_progress → return $ (progressResult <$> maybe_final_progress) == visitTreeUntilFirst visitor
                _ → fail $ "returned " ++ show termination_reason ++ " instead of WorkerFinished"
         -- }}}
        ]
     -- }}}
    ,testGroup "Visitor.Path" -- {{{
        [testGroup "sendTreeGeneratorDownPath" -- {{{
            [testCase "null path" $ (visitTree . sendTreeGeneratorDownPath Seq.empty) (return [42]) @?= [42]
            ,testCase "cache" $ do (visitTree . sendTreeGeneratorDownPath (Seq.singleton (CacheStep (encode ([42 :: Int]))))) (cache (undefined :: [Int])) @?= [42]
            ,testCase "cacheGuard" $ do (visitTree . sendTreeGeneratorDownPath (Seq.singleton (CacheStep (encode ())))) (cacheGuard False >> return [42::Int]) @?= [42]
            ,testCase "choice" $ do -- {{{
                (visitTree . sendTreeGeneratorDownPath (Seq.singleton (ChoiceStep LeftBranch))) (return [42] `mplus` undefined) @?= [42]
                (visitTree . sendTreeGeneratorDownPath (Seq.singleton (ChoiceStep RightBranch))) (undefined `mplus` return [42]) @?= [42]
             -- }}}
            ,testGroup "errors" -- {{{
                [testGroup "PastVisitorIsInconsistentWithPresentVisitor" -- {{{
                    [testCase "cache step with choice" $ -- {{{
                        try (
                            evaluate
                            .
                            visitTree
                            $
                            sendTreeGeneratorDownPath (Seq.singleton (CacheStep undefined :: Step)) (undefined `mplus` undefined :: TreeGenerator [Int])
                        ) >>= (@?= Left PastVisitorIsInconsistentWithPresentVisitor)
                     -- }}}
                    ,testCase "choice step with cache" $ -- {{{
                        try (
                            evaluate
                            .
                            visitTree
                            $
                            sendTreeGeneratorDownPath (Seq.singleton (ChoiceStep undefined :: Step)) (cache undefined :: TreeGenerator [Int])
                        ) >>= (@?= Left PastVisitorIsInconsistentWithPresentVisitor)
                     -- }}}
                    ]
                 -- }}}
                ,testGroup "VisitorTerminatedBeforeEndOfWalk" -- {{{
                    [testCase "mzero" $ -- {{{
                        try (
                            evaluate
                            .
                            visitTree
                            $
                            sendTreeGeneratorDownPath (Seq.singleton (undefined :: Step)) (mzero :: TreeGenerator [Int])
                        ) >>= (@?= Left VisitorTerminatedBeforeEndOfWalk)
                     -- }}}
                    ,testCase "return" $ -- {{{
                        try (
                            evaluate
                            .
                            visitTree
                            $
                            sendTreeGeneratorDownPath (Seq.singleton (undefined :: Step)) (return (undefined :: [Int]))
                        ) >>= (@?= Left VisitorTerminatedBeforeEndOfWalk)
                     -- }}}
                    ]
                 -- }}}
                ]
             -- }}}
            ]
         -- }}}
        ,testGroup "walkThroughTreeT" -- {{{
            [testCase "cache step" $ do -- {{{
                let (transformed_visitor,log) =
                        runWriter . sendTreeGeneratorTDownPath (Seq.singleton (CacheStep . encode $ [24 :: Int])) $ do
                            runAndCache (tell [1] >> return [42 :: Int] :: Writer [Int] [Int])
                log @?= []
                (runWriter . visitTreeT $ transformed_visitor) @?= ([24],[])
             -- }}}
            ,testCase "choice step" $ do -- {{{
                let (transformed_visitor,log) =
                        runWriter . sendTreeGeneratorTDownPath (Seq.singleton (ChoiceStep RightBranch)) $ do
                            lift (tell [1])
                            (lift (tell [2]) `mplus` lift (tell [3]))
                            lift (tell [4])
                            return [42]
                log @?= [1]
                (runWriter . visitTreeT $ transformed_visitor) @?= ([42],[3,4])
             -- }}}
            ]
         -- }}}
        ]
     -- }}}
    ,testGroup "Visitor.Utils.PerfectTree" -- {{{
        [Small.testProperty "trivialPerfectTree" . Small.test $ -- {{{
            (liftA2 . liftA2) (==>)
                (\arity _ → arity >= 2)
                ((liftA2 . liftA2) (==)
                    numberOfLeaves
                    ((getWordSum . visitTree) .* trivialPerfectTree)
                )
         -- }}}
        ]
     -- }}}
    ]
-- }}}
