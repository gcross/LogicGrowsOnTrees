-- Language extensions {{{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DoRec #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
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
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Operational (ProgramViewT(..),view)
import Control.Monad.STM
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT(..),ask)
import Control.Monad.Trans.State (StateT,evalStateT,get,modify)
import Control.Monad.Trans.Writer

import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.DList as DList
import Data.DList (DList)
import Data.Either.Unwrap
import qualified Data.Foldable as Fold
import Data.Function
import Data.Functor
import Data.Functor.Identity
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import Data.IORef
import Data.IVar (IVar)
import qualified Data.IVar as IVar
import Data.List (inits,mapAccumL,sort)
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Sequence (Seq,(<|),(|>),(><))
import qualified Data.Sequence as Seq
import qualified Data.Serialize as Serialize
import Data.Serialize (Serialize(),decode,encode)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Typeable
import qualified Data.UUID as UUID
import Data.UUID (UUID)
import Data.Word

import Debug.Trace (trace)

import System.Environment
import System.IO (stdin,stdout)
import System.IO.Unsafe
import System.Log.Logger (Priority(..),updateGlobalLogger,rootLoggerName,setLevel)
import qualified System.Log.Logger as Logger
import System.Log.Logger.TH
import System.Random

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit hiding (Path)
import Test.QuickCheck.Arbitrary hiding ((><))
import Test.QuickCheck.Gen
import Test.QuickCheck.Instances
import Test.QuickCheck.Modifiers
import Test.QuickCheck.Monadic
import Test.QuickCheck.Property hiding ((.&.))

import Control.Visitor
import Control.Visitor.Checkpoint
import Control.Visitor.Examples.Queens
import Control.Visitor.Label
import Control.Visitor.Main (RunOutcome(..),TerminationReason(..))
import qualified Control.Visitor.Parallel.Processes as Processes
import qualified Control.Visitor.Parallel.Threads as Threads
import qualified Control.Visitor.Parallel.Workgroup as Workgroup
import Control.Visitor.Path
import Control.Visitor.Workload
import Control.Visitor.Worker
import Control.Visitor.Supervisor
import Control.Visitor.Supervisor.RequestQueue
-- }}}

-- Helpers {{{
-- Instances {{{
-- Newtypes {{{
newtype UniqueVisitorT m = UniqueVisitor { unwrapUniqueVisitor :: VisitorT m IntSet }
-- }}}

-- Arbitrary {{{
instance Arbitrary Branch where arbitrary = elements [LeftBranch,RightBranch]

instance Arbitrary α ⇒ Arbitrary (DList α) where -- {{{
    arbitrary = DList.fromList <$> listOf arbitrary
-- }}}

instance Arbitrary UUID where -- {{{
    arbitrary = MkGen (\r _ -> fst (random r))
-- }}}

instance (Arbitrary α, Monoid α, Serialize α, Functor m, Monad m) ⇒ Arbitrary (VisitorT m α) where -- {{{
    arbitrary = fmap ($ mempty) (sized arb)
      where
        arb :: Monoid α ⇒ Int → Gen (α → VisitorT m α)
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

        null :: Gen (α → VisitorT m α)
        null = return (const mzero)

        result, cached :: Gen (VisitorT m α)
        result = fmap return arbitrary
        cached = fmap cache arbitrary

        resultPlus, cachedPlus :: Monoid α ⇒ Gen (α → VisitorT m α)
        resultPlus = (\x → flip fmap x . mappend) <$> result
        cachedPlus = (\x → flip fmap x . mappend) <$> cached
-- }}}

instance Monad m ⇒ Arbitrary (UniqueVisitorT m) where -- {{{
    arbitrary = fmap (UniqueVisitor . ($ 0)) (sized $ \n → evalStateT (arb n 0) IntSet.empty)
      where
        arb :: Int → Int → StateT IntSet Gen (Int → VisitorT m IntSet)
        arb 0 _ = return (const mzero)
        arb 1 intermediate = frequencyT
            [(1,return (const mzero))
            ,(3,generateUnique (return . IntSet.singleton) intermediate)
            ,(2,generateUnique (cache . IntSet.singleton) intermediate)
            ]
        arb n intermediate = frequencyT
            [(2,generateForNext return intermediate (arb n))
            ,(2,generateForNext cache intermediate (arb n))
            ,(4, do left_size ← lift $ choose (0,n)
                    let right_size = n-left_size
                    liftM2 (liftA2 mplus)
                        (arb left_size intermediate)
                        (arb right_size intermediate)
             )
            ]

        generateUnique :: Monad m ⇒ (Int → VisitorT m α) → Int → StateT IntSet Gen (Int → VisitorT m α)
        generateUnique construct intermediate = do
            observed ← get
            x ← lift (arbitrary `suchThat` (flip IntSet.notMember observed . (xor intermediate)))
            let final_value = x `xor` intermediate
            modify (IntSet.insert final_value)
            return $ construct . xor x

        generateForNext :: Monad m ⇒ (Int → VisitorT m α) → Int → (Int → StateT IntSet Gen (α → VisitorT m β)) → StateT IntSet Gen (Int → VisitorT m β)
        generateForNext construct intermediate next = do
            x ← lift arbitrary
            let new_intermediate = x `xor` intermediate
            fmap (construct . xor x >=>) $ next new_intermediate
-- }}}

instance Arbitrary Checkpoint where -- {{{
    arbitrary = sized arb
      where
        arb 0 = elements [Explored,Unexplored]
        arb n = frequency
                    [(1,return Explored)
                    ,(1,return Unexplored)
                    ,(1,liftM2 CacheCheckpoint (fmap encode (arbitrary :: Gen Int)) (arb (n-1)))
                    ,(2,liftM2 ChoiceCheckpoint (arb (n `div` 2)) (arb (n `div` 2)))
                    ]
-- }}}

instance Arbitrary VisitorLabel where arbitrary = fmap labelFromBranching (arbitrary :: Gen [Branch])

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
-- }}}

-- Functions {{{
addAcceptOneWorkloadAction :: -- {{{
    SupervisorCallbacks result worker_id IO →
    IO (IORef (Maybe (worker_id,Workload)),SupervisorCallbacks result worker_id IO)
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
    SupervisorCallbacks result worker_id IO →
    IO (IORef [(worker_id,Workload)],SupervisorCallbacks result worker_id IO)
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
    SupervisorCallbacks result worker_id IO →
    IO (IORef [[worker_id]],SupervisorCallbacks result worker_id IO)
addAppendWorkloadStealBroadcastIdsAction actions = do
    broadcasts_ref ← newIORef ([] :: [[worker_id]])
    return (broadcasts_ref, actions {
        broadcastWorkloadStealToWorkers = \worker_ids →
            modifyIORef broadcasts_ref (++ [worker_ids])
    })
-- }}}

addAppendProgressBroadcastIdsAction :: -- {{{
    SupervisorCallbacks result worker_id IO →
    IO (IORef [[worker_id]],SupervisorCallbacks result worker_id IO)
addAppendProgressBroadcastIdsAction actions = do
    broadcasts_ref ← newIORef ([] :: [[worker_id]])
    return (broadcasts_ref, actions {
        broadcastProgressUpdateToWorkers = \worker_ids →
            modifyIORef broadcasts_ref (++ [worker_ids])
    })
-- }}}

addReceiveCurrentProgressAction :: -- {{{
    SupervisorCallbacks result worker_id IO →
    IO (IORef (Maybe (Progress result)),SupervisorCallbacks result worker_id IO)
addReceiveCurrentProgressAction actions = do
    maybe_progress_ref ← newIORef (Nothing :: Maybe (Progress result))
    return (maybe_progress_ref, actions {
        receiveCurrentProgress = \progress → do
            maybe_old_progress ← readIORef maybe_progress_ref
            case maybe_old_progress of
                Nothing → return ()
                Just _ → error "progress update has been received already!"
            writeIORef maybe_progress_ref $ Just progress
    })
-- }}}

echo :: Show α ⇒ α → α -- {{{
echo x = trace (show x) x
-- }}}

echoWithLabel :: Show α ⇒ String → α → α -- {{{
echoWithLabel label x = trace (label ++ " " ++ show x) x
-- }}}

ignoreAcceptWorkloadAction :: -- {{{
    SupervisorCallbacks result worker_id IO →
    SupervisorCallbacks result worker_id IO
ignoreAcceptWorkloadAction actions = actions { sendWorkloadToWorker = \_ _ → return () }
-- }}}

ignoreWorkloadStealAction :: -- {{{
    SupervisorCallbacks result worker_id IO →
    SupervisorCallbacks result worker_id IO
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

randomCheckpointForVisitor :: Monoid α ⇒ Visitor α → Gen (α,Checkpoint) -- {{{
randomCheckpointForVisitor (VisitorT visitor) = go1 visitor
  where
    go1 visitor = frequency
        [(1,return (runVisitor (VisitorT visitor),Explored))
        ,(1,return (mempty,Unexplored))
        ,(3,go2 visitor)
        ]
    go2 (view → Cache (Identity (Just x)) :>>= k) =
        fmap (second $ CacheCheckpoint (encode x)) (go1 (k x))
    go2 (view → Choice (VisitorT x) (VisitorT y) :>>= k) =
        liftM2 (\(left_result,left) (right_result,right) →
            (left_result `mappend` right_result, ChoiceCheckpoint left right)
        ) (go1 (x >>= k)) (go1 (y >>= k))
    go2 visitor = elements [(runVisitor (VisitorT visitor),Explored),(mempty,Unexplored)]
-- }}}

randomPathForVisitor :: Visitor α → Gen Path -- {{{
randomPathForVisitor (VisitorT visitor) = go visitor
  where
    go (view → Cache (Identity (Just x)) :>>= k) = oneof
        [return Seq.empty
        ,fmap (CacheStep (encode x) <|) (go (k x))
        ]
    go (view → Choice (VisitorT x) (VisitorT y) :>>= k) = oneof
        [return Seq.empty
        ,fmap (ChoiceStep LeftBranch <|) (go (x >>= k))
        ,fmap (ChoiceStep RightBranch <|) (go (y >>= k))
        ]
    go _ = return Seq.empty
-- }}}

randomVisitorWithoutCache :: Arbitrary α ⇒ Gen (Visitor α) -- {{{
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
bad_test_supervisor_actions :: SupervisorCallbacks result worker_id m -- {{{
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
-- }}}

-- Loggers {{{
deriveLoggers "Logger" [ERROR]
-- }}}
-- }}}

main = do
    -- updateGlobalLogger rootLoggerName (setLevel DEBUG)
    args ← getArgs
    case args of
        ["nqueens",read → n] →
            Processes.runWorkerWithVisitor
                (nqueensCount n)
                stdin
                stdout
             `catch`
             (\(e::SomeException) → errorM $ "Worker process failed: " ++ show e)
        _ → defaultMain tests

tests = -- {{{
    [testGroup "test helpers" $ -- {{{
        [testProperty "UniqueVisitor has unique results" $ \(UniqueVisitor visitor) → -- {{{
            let results = runVisitor (fmap (:[]) visitor )
            in length results == IntSet.size (mconcat results)
         -- }}}
        ]
     -- }}}
    ,testGroup "Control.Visitor" -- {{{
        [testGroup "Eq instance" -- {{{
            [testProperty "self" $ \(v :: Visitor [()]) → v == v
            ]
         -- }}}
        ,testProperty "allFrom" $ \(x :: [Int]) → x == allFrom x
        ,testProperty "between" $ do -- {{{
            x ← choose ( 0,100) :: Gen Int
            y ← choose (50,100)
            return $ between x y == [x..y]
         -- }}}
        ,testProperty "msumBalanced" $ \(x :: [Int]) → x == msumBalanced (map return x)
        ,testProperty "msumBalancedGreedy" $ \(x :: [UUID]) → ((==) `on` sort) x (msumBalancedGreedy (map return x))
        ,testGroup "runVisitor" -- {{{
            [testCase "return" $ runVisitor (return [()]) @?= [()]
            ,testCase "mzero" $ runVisitor (mzero :: Visitor [()]) @?= []
            ,testCase "mplus" $ runVisitor (return [1] `mplus` return [2]) @?= [1,2]
            ,testCase "cache" $ runVisitor (cache [42]) @?= [42::Int]
            ,testGroup "cacheMaybe" -- {{{
                [testCase "Nothing" $ runVisitor (cacheMaybe (Nothing :: Maybe [()])) @?= []
                ,testCase "Just" $ runVisitor (cacheMaybe (Just [42])) @?= [42::Int]
                ]
             -- }}}
            ,testGroup "cacheGuard" -- {{{
                [testCase "True" $ runVisitor (cacheGuard False >> return [()]) @?= []
                ,testCase "False" $ runVisitor (cacheGuard True >> return [()]) @?= [()]
                ]
             -- }}}
            ]
         -- }}}
        ,testGroup "runVisitorT" -- {{{
            [testCase "Writer" $ -- {{{
                (runWriter . runVisitorT $ do
                    cache [1 :: Int] >>= lift . tell
                    (lift (tell [2]) `mplus` lift (tell [3]))
                    return [42]
                ) @?= ([42,42],[1,2,3])
             -- }}}
            ]
         -- }}}
        ,testGroup "runVisitorTAndIgnoreResults" -- {{{
            [testCase "Writer" $ -- {{{
                (runWriter . runVisitorTAndIgnoreResults $ do
                    cache [1 :: Int] >>= lift . tell
                    (lift (tell [2]) `mplus` lift (tell [3]))
                    return [42]
                ) @?= ((),[1,2,3])
             -- }}}
            ]
         -- }}}
        ]
     -- }}}
    ,testGroup "Control.Visitor.Checkpoint" -- {{{
        [testGroup "contextFromCheckpoint" -- {{{
            [testProperty "cache" $ \(checkpoint :: Checkpoint) (i :: Int) → -- {{{
                checkpointFromContext (Seq.singleton (CacheContextStep (encode i))) checkpoint
                ==
                (mergeCheckpointRoot $ CacheCheckpoint (encode i) checkpoint)
             -- }}}
            ,testProperty "left branch" $ \(inner_checkpoint :: Checkpoint) (other_visitor :: Visitor [()]) (other_checkpoint :: Checkpoint) → -- {{{
                (checkpointFromContext (Seq.singleton (LeftBranchContextStep other_checkpoint other_visitor)) inner_checkpoint)
                ==
                (mergeCheckpointRoot $ ChoiceCheckpoint inner_checkpoint other_checkpoint)
             -- }}}
            ,testProperty "right branch" $ \(checkpoint :: Checkpoint) → -- {{{
                checkpointFromContext (Seq.singleton RightBranchContextStep) checkpoint
                ==
                (mergeCheckpointRoot $ ChoiceCheckpoint Explored checkpoint)
             -- }}}
            ,testProperty "empty" $ \(checkpoint :: Checkpoint) → -- {{{
                checkpointFromContext Seq.empty checkpoint == checkpoint
             -- }}}
            ]
         -- }}}
        ,testProperty "invertCheckpoint" $ \(visitor :: Visitor (Set UUID)) → -- {{{
            randomCheckpointForVisitor visitor >>= \(partial_result,checkpoint) → return $
                partial_result == runVisitorThroughCheckpoint (invertCheckpoint checkpoint) visitor
         -- }}}
        ,testGroup "Monoid instance" -- {{{
            [testProperty "product results in intersection of solutions" $ \(UniqueVisitor visitor) → do -- {{{
                (_,checkpoint1) ← randomCheckpointForVisitor visitor
                (_,checkpoint2) ← randomCheckpointForVisitor visitor
                let checkpoint3 = checkpoint1 `mappend` checkpoint2
                    solutions1 = runVisitorThroughCheckpoint checkpoint1 visitor
                    solutions2 = runVisitorThroughCheckpoint checkpoint2 visitor
                    solutions3 = runVisitorThroughCheckpoint checkpoint3 visitor
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
                [((CacheCheckpoint (encode (42 :: Int)) Unexplored),(CacheCheckpoint (encode (42 :: Integer)) Unexplored))
                ,((ChoiceCheckpoint Unexplored Unexplored),CacheCheckpoint (encode (42 :: Int)) Unexplored)
                ]
             -- }}}
            ,testProperty "unit element laws" $ \(checkpoint :: Checkpoint) → -- {{{
                (mempty `mappend` checkpoint == checkpoint) && (checkpoint `mappend` mempty == checkpoint)
             -- }}}
            ]
         -- }}}
        ,testGroup "runVisitorThroughCheckpoint" -- {{{
            [testProperty "completes the solution space" $ \(UniqueVisitor visitor) → -- {{{
                randomCheckpointForVisitor visitor >>= \(partial_result,checkpoint) → return $
                    runVisitor visitor ==
                        mappend partial_result (runVisitorThroughCheckpoint checkpoint visitor)
             -- }}}
            ,testProperty "matches walkVisitorThroughCheckpoint" $ \(visitor :: Visitor [Int]) → do -- {{{
                (_,checkpoint) ← randomCheckpointForVisitor visitor
                morallyDubiousIOProperty $ do
                    runVisitorThroughCheckpoint checkpoint visitor
                        @?= (fst . last $ walkVisitorThroughCheckpoint checkpoint visitor)
                    return True
             -- }}}
            ]
         -- }}}
        ,testGroup "walkVisitorThroughCheckpoint" -- {{{
            [testProperty "matches walk down path" $ \(visitor :: Visitor [Int]) → randomPathForVisitor visitor >>= \path → return $ -- {{{
                runVisitor (sendVisitorDownPath path visitor)
                ==
                (fst . last) (walkVisitorThroughCheckpoint (checkpointFromUnexploredPath path) visitor)
             -- }}}
            ]
         -- }}}
        ,testGroup "walkVisitor" -- {{{
            [testProperty "last checkpoint is correct" $ \(v :: Visitor ()) → -- {{{
                let checkpoints = walkVisitor v
                in unsafePerformIO $ (last checkpoints @=? (runVisitor v,Explored)) >> return True
             -- }}}
            ,testProperty "checkpoints accurately capture remaining search space" $ \(UniqueVisitor v) → -- {{{
                let results_using_progressive_checkpoints =
                        [ result `mappend` runVisitorThroughCheckpoint checkpoint v
                        | (result,checkpoint) ← walkVisitor v
                        ]
                in all (== head results_using_progressive_checkpoints) (tail results_using_progressive_checkpoints)
             -- }}}
            ,testGroup "example instances" -- {{{
                [testGroup "mplus" -- {{{
                    [testCase "mzero + mzero" $ -- {{{
                        walkVisitor (mzero `mplus` mzero :: Visitor (Maybe ()))
                        @?=
                        [(Nothing,Unexplored)
                        ,(Nothing,ChoiceCheckpoint Explored Unexplored)
                        ,(Nothing,Explored)
                        ]
                     -- }}}
                    ,testCase "mzero + return" $ -- {{{
                        walkVisitor (mzero `mplus` return (Just ()) :: Visitor (Maybe ()))
                        @?=
                        [(Nothing,Unexplored)
                        ,(Nothing,ChoiceCheckpoint Explored Unexplored)
                        ,(Just (),Explored)
                        ]
                     -- }}}
                    ,testCase "return + mzero" $ -- {{{
                        walkVisitor (return (Just ()) `mplus` mzero :: Visitor (Maybe ()))
                        @?=
                        [(Nothing,Unexplored)
                        ,(Just (),ChoiceCheckpoint Explored Unexplored)
                        ,(Just (),Explored)
                        ]
                     -- }}}
                    ]
                 -- }}}
                ,testCase "mzero" $ walkVisitor (mzero :: Visitor [Int]) @?= [([],Explored)]
                ,testCase "return" $ walkVisitor (return [0] :: Visitor [Int]) @?= [([0],Explored)]
                ]
             -- }}}
            ]
         -- }}}
        ]
     -- }}}
    ,testGroup "Control.Visitor.Label" -- {{{
        [testProperty "branchingFromLabel . labelFromBranching = id" $ -- {{{
            liftA2 (==)
                (branchingFromLabel . labelFromBranching)
                id
         -- }}}
        ,testProperty "labelFromBranching . branchingFromLabel = id" $ -- {{{
            liftA2 (==)
                (labelFromBranching . branchingFromLabel)
                id
         -- }}}
        ,testGroup "Monoid instance" -- {{{
            [testProperty "equivalent to concatenation of branchings" $ \(parent_branching :: [Branch]) (child_branching :: [Branch]) → -- {{{
                labelFromBranching parent_branching `mappend` labelFromBranching child_branching
                ==
                labelFromBranching (parent_branching `mappend` child_branching)
             -- }}}
            ,testProperty "obeys monoid laws" $ -- {{{
                liftA2 (&&)
                    (liftA2 (==) id (`mappend` (mempty :: VisitorLabel)))
                    (liftA2 (==) id ((mempty :: VisitorLabel) `mappend`))
             -- }}}
            ]
         -- }}}
        ,testProperty "Ord instance of VisitorLabel equivalent to Ord of branching" $ \a b → -- {{{
            (compare `on` branchingFromLabel) a b == compare a b
         -- }}}
        ,testGroup "runVisitorWithLabels" -- {{{
            [testProperty "same result as runVisitor" $ \(visitor :: Visitor [()]) →
                 runVisitor ((:[]) <$> visitor) == (solutionResult <$> runVisitorWithLabels visitor)
            ]
         -- }}}
        ,testGroup "sendVisitorDownLabel" -- {{{
            [testProperty "same result as walking down path" $ do -- {{{
                visitor :: Visitor Int ← randomVisitorWithoutCache
                path ← randomPathForVisitor visitor
                let label = labelFromPath path
                return $
                    sendVisitorDownPath path visitor
                    ==
                    sendVisitorDownLabel label visitor
             -- }}}
            ]
         -- }}}
        ,testProperty "runLabelledVisitor" $ -- {{{
            let gen _ 0 = return mzero
                gen label 1 = return (All . (== label) <$> getLabel)
                gen label n = do
                    left_size ← choose (0,n)
                    let right_size = n-left_size
                    left ← gen (leftChildLabel label) left_size
                    right ← gen (rightChildLabel label) right_size
                    return $ left `mplus` right
            in getAll . runLabeledVisitor <$> sized (gen rootLabel)
         -- }}}
        ]
     -- }}}
    ,testGroup "Control.Visitor.Parallel.Processes" $ -- {{{
      let runTest generateNoise = do
             let visitor = nqueensCount 13
             progresses_ref ← newIORef []
             filepath ← Processes.getProgFilepath
             RunOutcome _ termination_reason ←
                 Processes.runSupervisor
                     filepath
                     ["nqueens","13"]
                     (const $ return ())
                     Nothing
                     (forever $ requestProgressUpdate >>= (liftIO . modifyIORef progresses_ref . (:)) >> generateNoise)
             result ← case termination_reason of
                 Aborted _ → error "prematurely aborted"
                 Completed result → return result
                 Failure message → error message
             let correct_result = runVisitor visitor
             result @?= correct_result
             progresses ← remdups <$> readIORef progresses_ref
             replicateM_ 4 $ randomRIO (0,length progresses-1) >>= \i → do
                 let Progress checkpoint result = progresses !! i
                 result @=? runVisitorThroughCheckpoint (invertCheckpoint checkpoint) visitor
                 correct_result @=? mappend result (runVisitorThroughCheckpoint checkpoint visitor)
      in [testCase "one process" . runTest . void $ do
              Workgroup.changeNumberOfWorkers (return . (\i → 0))
              Workgroup.changeNumberOfWorkers (return . (\i → 1))
        ,testCase "two processes" . runTest . void $
              Workgroup.changeNumberOfWorkers (return . (\i → 3-i))
        ,testCase "many processes" . runTest . void $ liftIO (randomRIO (0,1::Int)) >>= \i → case i of
          0 → Workgroup.changeNumberOfWorkers (return . (\i → if i > 1 then i-1 else i))
          1 → Workgroup.changeNumberOfWorkers (return . (+1))
        ]
     -- }}}
    ,testGroup "Control.Visitor.Parallel.Threads" $ -- {{{
        let runTest generateNoise = arbitrary >>= \(UniqueVisitor visitor) → morallyDubiousIOProperty $ do
                termination_reason_ivar ← IVar.new
                token_mvar ← newEmptyMVar
                request_mvar ← newEmptyMVar
                progresses_ref ← newIORef []
                let receiveProgress (Progress Unexplored _) = return ()
                    receiveProgress progress = atomicModifyIORef progresses_ref ((progress:) &&& const ())
                RunOutcome _ termination_reason ←
                    Threads.runVisitorIO
                        (do value ← endowVisitor visitor
                            liftIO $ putMVar request_mvar () >> takeMVar token_mvar
                            return value
                        )
                        .
                        forever
                        $
                        do Workgroup.changeNumberOfWorkers (const $ return 1)
                           liftIO $ takeMVar request_mvar
                           generateNoise receiveProgress
                           liftIO $ putMVar token_mvar ()
                result ← case termination_reason of
                    Aborted _ → error "prematurely aborted"
                    Completed result → return result
                    Failure message → error message
                let correct_result = runVisitor visitor
                result @?= correct_result
                (remdups <$> readIORef progresses_ref) >>= mapM_ (\(Progress checkpoint result) → do
                    result @=? runVisitorThroughCheckpoint (invertCheckpoint checkpoint) visitor
                    correct_result @=? mappend result (runVisitorThroughCheckpoint checkpoint visitor)
                 )
                return True
      in
      [testProperty "one thread" . runTest $ \receiveProgress → liftIO (randomRIO (0,1::Int)) >>= \i → case i of
          0 → void $ do
                  Workgroup.changeNumberOfWorkers (return . (\i → 0))
                  Workgroup.changeNumberOfWorkers (return . (\i → 1))
          1 → void $ requestProgressUpdateAsync receiveProgress
      ,testProperty "two threads" . runTest $ \receiveProgress → liftIO (randomRIO (0,1::Int)) >>= \i → case i of
          0 → void $ Workgroup.changeNumberOfWorkers (return . (\i → 3-i))
          1 → void $ requestProgressUpdateAsync receiveProgress
      ,testProperty "many threads" . runTest $ \receiveProgress → liftIO (randomRIO (0,2::Int)) >>= \i → case i of
          0 → void $ Workgroup.changeNumberOfWorkers (return . (\i → if i > 1 then i-1 else i))
          1 → void $ Workgroup.changeNumberOfWorkers (return . (+1))
          2 → void $ requestProgressUpdateAsync receiveProgress
      ]
     -- }}}
    ,testGroup "Control.Visitor.Path" -- {{{
        [testGroup "sendVisitorDownPath" -- {{{
            [testCase "null path" $ (runVisitor . sendVisitorDownPath Seq.empty) (return [42]) @?= [42]
            ,testCase "cache" $ do (runVisitor . sendVisitorDownPath (Seq.singleton (CacheStep (encode ([42 :: Int]))))) (cache (undefined :: [Int])) @?= [42]
            ,testCase "cacheGuard" $ do (runVisitor . sendVisitorDownPath (Seq.singleton (CacheStep (encode ())))) (cacheGuard False >> return [42::Int]) @?= [42]
            ,testCase "choice" $ do -- {{{
                (runVisitor . sendVisitorDownPath (Seq.singleton (ChoiceStep LeftBranch))) (return [42] `mplus` undefined) @?= [42]
                (runVisitor . sendVisitorDownPath (Seq.singleton (ChoiceStep RightBranch))) (undefined `mplus` return [42]) @?= [42]
             -- }}}
            ,testGroup "errors" -- {{{
                [testGroup "PastVisitorIsInconsistentWithPresentVisitor" -- {{{
                    [testCase "cache step with choice" $ -- {{{
                        try (
                            evaluate
                            .
                            runVisitor
                            $
                            sendVisitorDownPath (Seq.singleton (CacheStep undefined :: Step)) (undefined `mplus` undefined :: Visitor [Int])
                        ) >>= (@?= Left PastVisitorIsInconsistentWithPresentVisitor)
                     -- }}}
                    ,testCase "choice step with cache" $ -- {{{
                        try (
                            evaluate
                            .
                            runVisitor
                            $
                            sendVisitorDownPath (Seq.singleton (ChoiceStep undefined :: Step)) (cache undefined :: Visitor [Int])
                        ) >>= (@?= Left PastVisitorIsInconsistentWithPresentVisitor)
                     -- }}}
                    ]
                 -- }}}
                ,testGroup "VisitorTerminatedBeforeEndOfWalk" -- {{{
                    [testCase "mzero" $ -- {{{
                        try (
                            evaluate
                            .
                            runVisitor
                            $
                            sendVisitorDownPath (Seq.singleton (undefined :: Step)) (mzero :: Visitor [Int])
                        ) >>= (@?= Left VisitorTerminatedBeforeEndOfWalk)
                     -- }}}
                    ,testCase "return" $ -- {{{
                        try (
                            evaluate
                            .
                            runVisitor
                            $
                            sendVisitorDownPath (Seq.singleton (undefined :: Step)) (return (undefined :: [Int]))
                        ) >>= (@?= Left VisitorTerminatedBeforeEndOfWalk)
                     -- }}}
                    ]
                 -- }}}
                ]
             -- }}}
            ]
         -- }}}
        ,testGroup "walkVisitorT" -- {{{
            [testCase "cache step" $ do -- {{{
                let (transformed_visitor,log) =
                        runWriter . sendVisitorTDownPath (Seq.singleton (CacheStep . encode $ [24 :: Int])) $ do
                            runAndCache (tell [1] >> return [42 :: Int] :: Writer [Int] [Int])
                log @?= []
                (runWriter . runVisitorT $ transformed_visitor) @?= ([24],[])
             -- }}}
            ,testCase "choice step" $ do -- {{{
                let (transformed_visitor,log) =
                        runWriter . sendVisitorTDownPath (Seq.singleton (ChoiceStep RightBranch)) $ do
                            lift (tell [1])
                            (lift (tell [2]) `mplus` lift (tell [3]))
                            lift (tell [4])
                            return [42]
                log @?= [1]
                (runWriter . runVisitorT $ transformed_visitor) @?= ([42],[3,4])
             -- }}}
            ]
         -- }}}
        ]
     -- }}}
    ,testGroup "Control.Visitor.Supervisor" -- {{{
        [testCase "immediately abort" $ do -- {{{
            SupervisorOutcome{..} ← runSupervisor bad_test_supervisor_actions (UnrestrictedProgram abortSupervisor)
            supervisorTerminationReason @?= SupervisorAborted (Progress Unexplored ())
            supervisorRemainingWorkers @?= ([] :: [Int])
         -- }}}
        ,testCase "failure" $ do -- {{{
            SupervisorOutcome{..} ← runUnrestrictedSupervisor bad_test_supervisor_actions (receiveWorkerFailure () "FAIL" :: ∀ α. SupervisorMonad () () IO α)
            supervisorTerminationReason @?= SupervisorFailure () "FAIL"
            supervisorRemainingWorkers @?= []
         -- }}}
        ,testGroup "adding and removing workers" -- {{{
            [testCase "add one worker then abort" $ do -- {{{
                (maybe_workload_ref,actions) ← addAcceptOneWorkloadAction bad_test_supervisor_actions
                SupervisorOutcome{..} ← runUnrestrictedSupervisor actions $ do
                    enableSupervisorDebugMode
                    addWorker ()
                    abortSupervisor
                supervisorTerminationReason @?= SupervisorAborted (Progress Unexplored ())
                supervisorRemainingWorkers @?= [()]
                readIORef maybe_workload_ref >>= (@?= Just ((),entire_workload))
             -- }}}
            ,testCase "add then remove one worker then abort" $ do -- {{{
                (maybe_workload_ref,actions) ← addAcceptOneWorkloadAction bad_test_supervisor_actions
                SupervisorOutcome{..} ← runUnrestrictedSupervisor actions $ do
                    enableSupervisorDebugMode
                    addWorker ()
                    removeWorker ()
                    abortSupervisor
                supervisorTerminationReason @?= SupervisorAborted (Progress Unexplored ())
                supervisorRemainingWorkers @?= []
                readIORef maybe_workload_ref >>= (@?= Just ((),entire_workload)) 
             -- }}}
            ,testCase "add then remove then add one worker then abort" $ do -- {{{
                (maybe_workload_ref,actions) ← addAcceptMultipleWorkloadsAction bad_test_supervisor_actions
                SupervisorOutcome{..} ← runUnrestrictedSupervisor actions $ do
                    enableSupervisorDebugMode
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
                SupervisorOutcome{..} ← runUnrestrictedSupervisor actions2 $ do
                    enableSupervisorDebugMode
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
                    SupervisorOutcome{..} ← runUnrestrictedSupervisor actions_2 $ do
                        enableSupervisorDebugMode
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
        ,testGroup "progress updates" -- {{{
            [testCase "request progress update when no workers present" $ do -- {{{
                (maybe_progress_ref,actions) ← addReceiveCurrentProgressAction bad_test_supervisor_actions
                SupervisorOutcome{..} ← runUnrestrictedSupervisor actions $ do
                    enableSupervisorDebugMode
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
                    let actions3 = ignoreAcceptWorkloadAction . ignoreWorkloadStealAction $ actions2
                    let progress = Progress Unexplored (Sum 0)
                    SupervisorOutcome{..} ← runUnrestrictedSupervisor actions3 $ do
                        addWorker 0
                        forM_ (zip [0..] (tail active_workers)) $ \(prefix_count,worker_id) → do
                            addWorker worker_id
                            let remaining_workload = Workload (Seq.replicate (prefix_count+1) (ChoiceStep LeftBranch)) Unexplored
                            let stolen_workload = Workload (Seq.replicate (prefix_count) (ChoiceStep LeftBranch) |> (ChoiceStep RightBranch)) Unexplored
                            receiveStolenWorkload 0 $ Just (StolenWorkload (ProgressUpdate mempty remaining_workload) stolen_workload)
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
                let progress = Progress (ChoiceCheckpoint Unexplored Unexplored) (Sum 1)
                SupervisorOutcome{..} ← runUnrestrictedSupervisor actions3 $ do
                    enableSupervisorDebugMode
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
                let progress = Progress (ChoiceCheckpoint Unexplored Unexplored) (Sum 1)
                SupervisorOutcome{..} ← runUnrestrictedSupervisor actions3 $ do
                    enableSupervisorDebugMode
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
                SupervisorOutcome{..} ← runUnrestrictedSupervisor actions2 $ do
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
            (maybe_workload_ref,actions) ← addAcceptOneWorkloadAction bad_test_supervisor_actions
            let checkpoint = ChoiceCheckpoint Unexplored Unexplored
                progress = Progress checkpoint (Sum 1)
            SupervisorOutcome{..} ← runUnrestrictedSupervisorStartingFrom progress actions $ do
                addWorker ()
                abortSupervisor
            supervisorTerminationReason @?= SupervisorAborted progress
            supervisorRemainingWorkers @?= [()]
            readIORef maybe_workload_ref >>= (@?= Just ((),(Workload Seq.empty checkpoint)))
         -- }}}
        ]
     -- }}}
    ,testGroup "Control.Visitor.Worker" -- {{{
        [testGroup "forkVisitor(X)WorkerThread" -- {{{
            [testCase "abort" $ do -- {{{
                termination_result_ivar ← IVar.new
                semaphore ← newEmptyMVar
                WorkerEnvironment{..} ← forkVisitorIOWorkerThread
                    (IVar.write termination_result_ivar)
                    (liftIO (takeMVar semaphore) `mplus` error "should never get here")
                    entire_workload
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
                [testProperty "with no initial path" $ \(visitor :: Visitor [Int]) → unsafePerformIO $ do -- {{{
                    solutions_ivar ← IVar.new
                    worker_environment ←
                        forkVisitorWorkerThread
                            (IVar.write solutions_ivar)
                            visitor
                            entire_workload
                    Progress checkpoint solutions ←
                        (IVar.blocking $ IVar.read solutions_ivar)
                        >>=
                        \termination_reason → case termination_reason of
                            WorkerFinished final_progress → return final_progress
                            other → error ("terminated unsuccessfully with reason " ++ show other)
                    checkpoint @?= Explored
                    solutions @?= runVisitor visitor
                    return True
                 -- }}}
                ,testProperty "with an initial path" $ \(visitor :: Visitor [Int]) → randomPathForVisitor visitor >>= \path → return . unsafePerformIO $ do -- {{{
                    solutions_ivar ← IVar.new
                    worker_environment ←
                        forkVisitorWorkerThread
                            (IVar.write solutions_ivar)
                            visitor
                            (Workload path Unexplored)
                    Progress checkpoint solutions ←
                        (IVar.blocking $ IVar.read solutions_ivar)
                        >>=
                        \termination_reason → case termination_reason of
                            WorkerFinished final_progress → return final_progress
                            other → error ("terminated unsuccessfully with reason " ++ show other)
                    checkpoint @?= checkpointFromInitialPath path Explored
                    solutions @?= (runVisitor . sendVisitorDownPath path $ visitor)
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
                        let correct_solutions = runVisitor visitor
                            update_solutions = map (progressResult . progressUpdateProgress) progress_updates
                            all_solutions = remaining_solutions:update_solutions
                        forM_ (zip [0..] all_solutions) $ \(i,solutions_1) →
                            forM_ (zip [0..] all_solutions) $ \(j,solutions_2) →
                                unless (i == j) $
                                    assertBool "Is there an overlap between non-intersecting solutions?"
                                        (IntSet.null $ solutions_1 `IntSet.intersection` solutions_2)
                        let total_update_solutions = mconcat update_solutions
                            total_solutions = mconcat all_solutions
                        assertEqual "Are the total solutions correct?"
                            correct_solutions
                            total_solutions
                        let accumulated_update_solutions = scanl1 mappend update_solutions
                        sequence_ $
                            zipWith (\accumulated_solutions (ProgressUpdate (Progress checkpoint _) remaining_workload) → do
                                let remaining_solutions = runVisitorThroughWorkload remaining_workload visitor
                                assertBool "Is there overlap between the accumulated solutions and the remaining solutions?"
                                    (IntSet.null $ accumulated_solutions `IntSet.intersection` remaining_solutions)
                                assertEqual "Do the accumulated and remaining solutions sum to the correct solutions?"
                                    correct_solutions
                                    (accumulated_solutions `mappend` remaining_solutions)
                                assertEqual "Is the checkpoint equal to the the remaining solutions?"
                                    remaining_solutions
                                    (runVisitorThroughCheckpoint checkpoint visitor)
                                assertEqual "Is the inverted checkpoint equal to the the accumulated solutions?"
                                    accumulated_solutions
                                    (runVisitorThroughCheckpoint (invertCheckpoint checkpoint) visitor)
                             ) accumulated_update_solutions progress_updates
                        return True
                in -- }}}
                [testProperty "continuous progress update requests" $ \(UniqueVisitor visitor) → unsafePerformIO $ do -- {{{
                    starting_flag ← IVar.new
                    termination_result_ivar ← IVar.new
                    WorkerEnvironment{..} ← forkVisitorIOWorkerThread
                        (IVar.write termination_result_ivar)
                        ((liftIO . IVar.blocking . IVar.read $ starting_flag) >> endowVisitor visitor)
                        entire_workload
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
                    rec WorkerEnvironment{..} ← forkVisitorIOWorkerThread
                            (IVar.write termination_result_ivar)
                            (do value ← endowVisitor visitor
                                liftIO $ randomIO >>= flip when submitMyProgressUpdateRequest
                                return value
                            )
                            entire_workload
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
                    forkVisitorWorkerThread
                        (IVar.write termination_result_ivar)
                        (mzero :: Visitor [Int])
                        entire_workload
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
                        (Progress checkpoint remaining_solutions) ← case termination_result of
                            WorkerFinished final_progress → return final_progress
                            WorkerFailed exception → error ("worker threw exception: " ++ show exception)
                            WorkerAborted → error "worker aborted prematurely"
                        (IVar.nonblocking . IVar.read) termination_flag >>= assertBool "is the termination flag set?" . isJust
                        steals ← reverse <$> readIORef steals_ref
                        let correct_solutions = runVisitor visitor
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
                                    flip runVisitorThroughWorkload visitor
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
                            let remaining_solutions = runVisitorThroughWorkload remaining_workload visitor
                                accumulated_solutions = acc_prestolen `mappend` acc_stolen
                            assertBool "Is there overlap between the accumulated solutions and the remaining solutions?"
                                (IntSet.null $ accumulated_solutions `IntSet.intersection` remaining_solutions)
                            assertEqual "Do the accumulated and remaining solutions sum to the correct solutions?"
                                correct_solutions
                                (accumulated_solutions `mappend` remaining_solutions)
                            assertEqual "Is the checkpoint equal to the stolen plus the remaining solutions?"
                                (acc_stolen `mappend` remaining_solutions)
                                (runVisitorThroughCheckpoint checkpoint visitor)
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
                                        tryPutMVar reached_position_mvar ()
                                        IVar.blocking . IVar.read $ blocking_value_ivar
                                    )
                                    (return (IntSet.singleton 101010101))
                                )
                                (endowVisitor visitor)
                    termination_result_ivar ← IVar.new
                    WorkerEnvironment{..} ← forkVisitorIOWorkerThread
                        (IVar.write termination_result_ivar)
                        visitor_with_blocking_value
                        entire_workload
                    maybe_workload_ref ← newIORef Nothing
                    takeMVar reached_position_mvar
                    sendWorkloadStealRequest workerPendingRequests $ writeIORef maybe_workload_ref
                    IVar.write blocking_value_ivar (IntSet.singleton 202020202)
                    final_progress@(Progress checkpoint remaining_solutions) ←
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
                    assertBool "Does the checkpoint have unexplored nodes?" $ mergeAllCheckpointNodes checkpoint /= Explored
                    runVisitorTThroughWorkload remaining_workload visitor_with_blocking_value >>= (remaining_solutions @?=)
                    runVisitorTThroughCheckpoint (invertCheckpoint checkpoint) visitor_with_blocking_value >>= (prestolen_solutions @?=)
                    correct_solutions ← runVisitorT visitor_with_blocking_value
                    stolen_solutions ← runVisitorTThroughWorkload stolen_workload visitor_with_blocking_value
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
                    WorkerEnvironment{..} ← forkVisitorIOWorkerThread
                        (IVar.write termination_result_ivar)
                        ((liftIO . IVar.blocking . IVar.read $ starting_flag) >> endowVisitor visitor)
                        entire_workload
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
                    rec WorkerEnvironment{..} ← forkVisitorIOWorkerThread
                            (IVar.write termination_result_ivar)
                            (do value ← endowVisitor visitor
                                liftIO $ randomIO >>= flip when submitMyWorkloadStealRequest
                                return value
                            )
                            entire_workload
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
        ]
     -- }}}
    ]
-- }}}