-- Language extensions {{{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- }}}

-- Imports {{{
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.QSem
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Operational (ProgramViewT(..),view)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Writer

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.DList as DList
import Data.DList (DList)
import Data.Either.Unwrap
import qualified Data.Foldable as Fold
import Data.Function
import Data.Functor
import Data.Functor.Identity
import Data.IORef
import Data.IVar (IVar)
import qualified Data.IVar as IVar
import Data.List (inits,mapAccumL,sort)
import Data.Maybe
import Data.Monoid
import Data.Monoid.Unicode
import Data.Sequence (Seq,(<|),(|>),(><))
import qualified Data.Sequence as Seq
import qualified Data.Serialize as Serialize
import Data.Serialize (Serialize(..),decode,encode)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Typeable
import qualified Data.UUID as UUID
import Data.UUID (UUID)

import Debug.Trace (trace)

import System.IO.Unsafe
import System.Random

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck.Arbitrary hiding ((><))
import Test.QuickCheck.Gen
import Test.QuickCheck.Modifiers
import Test.QuickCheck.Monadic
import Test.QuickCheck.Property

import Control.Monad.Trans.Visitor
import Control.Monad.Trans.Visitor.Checkpoint
import Control.Monad.Trans.Visitor.Label
import Control.Monad.Trans.Visitor.Path
import Control.Monad.Trans.Visitor.Workload
import Control.Monad.Trans.Visitor.Worker
import Control.Monad.Trans.Visitor.Supervisor
-- }}}

-- Helpers {{{
-- Instances {{{
-- Arbitrary {{{
instance Arbitrary Branch where arbitrary = elements [LeftBranch,RightBranch]

instance (Arbitrary α, Ord α) ⇒ Arbitrary (Set α) where -- {{{
    arbitrary = fmap Set.fromList (listOf arbitrary)
-- }}}

instance Arbitrary α ⇒ Arbitrary (DList α) where -- {{{
    arbitrary = DList.fromList <$> listOf arbitrary
-- }}}

instance Arbitrary UUID where -- {{{
    arbitrary = MkGen (\r _ -> fst (random r))
-- }}}

instance (Arbitrary α, Monoid α, Serialize α, Functor m, Monad m) ⇒ Arbitrary (VisitorT m α) where -- {{{
    arbitrary = sized (flip arb mzero)
      where
        arb :: Monoid α ⇒ Int → VisitorT m α → Gen (VisitorT m α)
        arb 0 _ = null
        arb 1 intermediate = frequency
                    [(3,resultPlus intermediate)
                    ,(1,null)
                    ,(2,cachedPlus intermediate)
                    ]
        arb n intermediate = frequency
                    [(2,resultPlus intermediate >>= arb n)
                    ,(1,null)
                    ,(2,cachedPlus intermediate >>= arb n)
                    ,(4, do left_size ← choose (0,n)
                            let right_size = n-left_size
                            left ← arb left_size intermediate
                            right ← arb right_size intermediate
                            return $ left `mplus` right
                     )
                    ]
        null, result, cached :: Gen (VisitorT m α)
        null = return mzero
        result = fmap return arbitrary
        cached = fmap cache arbitrary

        resultPlus, cachedPlus :: Monoid α ⇒ VisitorT m α → Gen (VisitorT m α)
        resultPlus intermediate = fmap (liftM2 mappend intermediate) result
        cachedPlus intermediate = fmap (liftM2 mappend intermediate) cached
-- }}}

instance Arbitrary VisitorCheckpoint where -- {{{
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

instance Arbitrary VisitorPath where -- {{{
    arbitrary = fmap Seq.fromList . listOf . oneof $ [fmap (CacheStep . encode) (arbitrary :: Gen Int),fmap ChoiceStep arbitrary]
-- }}}

instance Arbitrary α ⇒ Arbitrary (VisitorSolution α) where -- {{{
    arbitrary = VisitorSolution <$> arbitrary <*> arbitrary
-- }}}

instance Arbitrary α ⇒ Arbitrary (VisitorProgress α) where -- {{{
    arbitrary = VisitorProgress <$> arbitrary <*> arbitrary
-- }}}

instance Arbitrary α ⇒ Arbitrary (VisitorWorkerProgressUpdate α) where -- {{{
    arbitrary = VisitorWorkerProgressUpdate <$> arbitrary <*> arbitrary
-- }}}

instance Arbitrary α ⇒ Arbitrary (VisitorWorkerStolenWorkload α) where -- {{{
    arbitrary = VisitorWorkerStolenWorkload <$> arbitrary <*> arbitrary
-- }}}

instance Arbitrary VisitorWorkload where -- {{{
    arbitrary = VisitorWorkload <$> arbitrary <*> arbitrary
-- }}}
-- }}} Arbitrary

-- Serialize {{{
instance Serialize α ⇒ Serialize (DList α) where -- {{{
    put = Serialize.putListOf Serialize.put . DList.toList
    get = DList.fromList <$> Serialize.getListOf Serialize.get
-- }}}

instance Serialize UUID where -- {{{
    put = Serialize.putLazyByteString . UUID.toByteString
    get = fromJust . UUID.fromByteString <$> Serialize.getLazyByteString 16
-- }}}
-- }}} Serialize

instance Eq α ⇒ Eq (DList α) where -- {{{
    (==) = (==) `on` DList.toList
-- }}}

instance Show α ⇒ Show (DList α) where -- {{{
    show = ("DList.fromList " ++) . show . DList.toList
-- }}}
-- }}} Instances

-- Exceptions {{{
-- TestException {{{
data TestException = TestException Int deriving (Eq,Show,Typeable)
instance Exception TestException
-- }}}
-- }}} Exceptions

-- Functions {{{
addAcceptOneWorkloadAction :: -- {{{
    VisitorSupervisorActions result worker_id IO →
    IO (IORef (Maybe (worker_id,VisitorWorkload)),VisitorSupervisorActions result worker_id IO)
addAcceptOneWorkloadAction actions = do
    maybe_worker_and_workload_ref ← newIORef (Nothing :: Maybe (worker_id,VisitorWorkload))
    return (maybe_worker_and_workload_ref, actions {
        send_workload_to_worker_action = \workload worker_id → do
            maybe_old_workload ← readIORef maybe_worker_and_workload_ref
            case maybe_old_workload of
                Nothing → return ()
                Just _ → error "workload has been submitted already!"
            writeIORef maybe_worker_and_workload_ref $ Just (worker_id,workload)
    })
-- }}}

addAcceptMultipleWorkloadsAction :: -- {{{
    VisitorSupervisorActions result worker_id IO →
    IO (IORef [(worker_id,VisitorWorkload)],VisitorSupervisorActions result worker_id IO)
addAcceptMultipleWorkloadsAction actions = do
    workers_and_workloads_ref ← newIORef []
    return (workers_and_workloads_ref, actions {
        send_workload_to_worker_action = \workload worker_id →
            readIORef workers_and_workloads_ref
            >>=
            writeIORef workers_and_workloads_ref . (++ [(worker_id,workload)])
    })
-- }}}

addAppendWorkloadStealBroadcastIdsAction :: -- {{{
    VisitorSupervisorActions result worker_id IO →
    IO (IORef [[worker_id]],VisitorSupervisorActions result worker_id IO)
addAppendWorkloadStealBroadcastIdsAction actions = do
    broadcasts_ref ← newIORef ([] :: [[worker_id]])
    return (broadcasts_ref, actions {
        broadcast_workload_steal_to_workers_action = \worker_ids →
            modifyIORef broadcasts_ref (++ [worker_ids])
    })
-- }}}

addAppendProgressBroadcastIdsAction :: -- {{{
    VisitorSupervisorActions result worker_id IO →
    IO (IORef [[worker_id]],VisitorSupervisorActions result worker_id IO)
addAppendProgressBroadcastIdsAction actions = do
    broadcasts_ref ← newIORef ([] :: [[worker_id]])
    return (broadcasts_ref, actions {
        broadcast_progress_update_to_workers_action = \worker_ids →
            modifyIORef broadcasts_ref (++ [worker_ids])
    })
-- }}}

addReceiveCurrentProgressAction :: -- {{{
    VisitorSupervisorActions result worker_id IO →
    IO (IORef (Maybe (VisitorProgress result)),VisitorSupervisorActions result worker_id IO)
addReceiveCurrentProgressAction actions = do
    maybe_progress_ref ← newIORef (Nothing :: Maybe (VisitorProgress result))
    return (maybe_progress_ref, actions {
        receive_current_progress_action = \progress → do
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
    VisitorSupervisorActions result worker_id IO →
    VisitorSupervisorActions result worker_id IO
ignoreAcceptWorkloadAction actions = actions { send_workload_to_worker_action = \_ _ → return () }
-- }}}

ignoreWorkloadStealAction :: -- {{{
    VisitorSupervisorActions result worker_id IO →
    VisitorSupervisorActions result worker_id IO
ignoreWorkloadStealAction actions = actions { broadcast_workload_steal_to_workers_action = \_ → return () }
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

randomCheckpointForVisitor :: Visitor α → Gen VisitorCheckpoint -- {{{
randomCheckpointForVisitor (VisitorT visitor) = go1 visitor
  where
    go1 visitor = frequency
        [(1,return Explored)
        ,(1,return Unexplored)
        ,(3,go2 visitor)
        ]
    go2 (view → Cache (Identity (Just x)) :>>= k) =
        fmap (CacheCheckpoint (encode x)) (go1 (k x))
    go2 (view → Choice (VisitorT x) (VisitorT y) :>>= k) =
        liftM2 ChoiceCheckpoint (go1 (x >>= k)) (go1 (y >>= k))
    go2 _ = elements [Explored,Unexplored]
-- }}}

randomPathForVisitor :: Visitor α → Gen VisitorPath -- {{{
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
-- }}} Functions

-- Values {{{
bad_test_supervisor_actions :: VisitorSupervisorActions result worker_id m -- {{{
bad_test_supervisor_actions =
    VisitorSupervisorActions
    {   broadcast_progress_update_to_workers_action =
            error "broadcast_progress_update_to_workers_action called! :-/"
    ,   broadcast_workload_steal_to_workers_action =
            error "broadcast_workload_steal_to_workers_action called! :-/"
    ,   receive_current_progress_action =
            error "receive_current_progress_action called! :-/"
    ,   send_workload_to_worker_action =
            error "send_workload_to_worker_action called! :-/"
    }
-- }}}
-- }}}
-- }}} Helpers

main = defaultMain tests

tests = -- {{{
    [testGroup "Control.Monad.Trans.Visitor" -- {{{
        [testGroup "Eq instance" -- {{{
            [testProperty "self" $ \(v :: Visitor [()]) → v == v
            ]
         -- }}}
        ,testGroup "runVisitor" -- {{{
            [testCase "return" $ runVisitor (return [()]) @?= [()]
            ,testCase "mzero" $ runVisitor (mzero :: Visitor [()]) @?= []
            ,testCase "mplus" $ runVisitor (return [1] ⊕ return [2]) @?= [1,2]
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
    ,testGroup "Control.Monad.Trans.Visitor.Checkpoint" -- {{{
        [testGroup "contextFromCheckpoint" -- {{{
            [testProperty "cache" $ \(checkpoint :: VisitorCheckpoint) (i :: Int) → -- {{{
                checkpointFromContext (Seq.singleton (CacheContextStep (encode i))) checkpoint
                ==
                (mergeCheckpointRoot $ CacheCheckpoint (encode i) checkpoint)
             -- }}}
            ,testProperty "left branch" $ \(inner_checkpoint :: VisitorCheckpoint) (other_visitor :: Visitor [()]) (other_checkpoint :: VisitorCheckpoint) → -- {{{
                (checkpointFromContext (Seq.singleton (LeftBranchContextStep other_checkpoint other_visitor)) inner_checkpoint)
                ==
                (mergeCheckpointRoot $ ChoiceCheckpoint inner_checkpoint other_checkpoint)
             -- }}}
            ,testProperty "right branch" $ \(checkpoint :: VisitorCheckpoint) → -- {{{
                checkpointFromContext (Seq.singleton RightBranchContextStep) checkpoint
                ==
                (mergeCheckpointRoot $ ChoiceCheckpoint Explored checkpoint)
             -- }}}
            ,testProperty "empty" $ \(checkpoint :: VisitorCheckpoint) → -- {{{
                checkpointFromContext Seq.empty checkpoint == checkpoint
             -- }}}
            ]
         -- }}}
        ,testGroup "Monoid instance" -- {{{
            [testProperty "product results in intersection of solutions" $ \(visitor :: Visitor (Set UUID)) → do -- {{{
                checkpoint1 ← randomCheckpointForVisitor visitor
                checkpoint2 ← randomCheckpointForVisitor visitor
                let checkpoint3 = checkpoint1 ⊕ checkpoint2
                    solutions1 = runVisitorThroughCheckpoint checkpoint1 visitor
                    solutions2 = runVisitorThroughCheckpoint checkpoint2 visitor
                    solutions3 = runVisitorThroughCheckpoint checkpoint3 visitor
                return $ solutions3 == solutions1 `Set.intersection` solutions2
             -- }}}
            ,testCase "throws the correct exceptions" $ -- {{{
                mapM_ (\(x,y) →
                    try (
                        evaluate (x ⊕ y)
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
            ,testProperty "unit element laws" $ \(checkpoint :: VisitorCheckpoint) → -- {{{
                mempty ⊕ checkpoint == checkpoint && checkpoint ⊕ mempty == checkpoint
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
            ,testProperty "checkpoints accurately capture remaining search space" $ \(v :: Visitor [Int]) → -- {{{
                let results_using_progressive_checkpoints =
                        [result ⊕ runVisitorThroughCheckpoint checkpoint v
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
    ,testGroup "Control.Monad.Trans.Visitor.Label" -- {{{
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
                labelFromBranching parent_branching ⊕ labelFromBranching child_branching
                ==
                labelFromBranching (parent_branching ⊕ child_branching)
             -- }}}
            ,testProperty "obeys monoid laws" $ -- {{{
                liftA2 (&&)
                    (liftA2 (==) id (⊕ (mempty :: VisitorLabel)))
                    (liftA2 (==) id ((mempty :: VisitorLabel) ⊕))
             -- }}}
            ]
         -- }}}
        ,testProperty "Ord instance of VisitorLabel equivalent to Ord of branching" $ \a b → -- {{{
            (compare `on` branchingFromLabel) a b == compare a b
         -- }}}
        ,testGroup "runVisitorWithLabels" -- {{{
            [testProperty "same result as runVisitor" $ \(visitor :: Visitor [()]) →
                 runVisitor ((:[]) <$> visitor) == (visitorSolutionResult <$> runVisitorWithLabels visitor)
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
    ,testGroup "Control.Monad.Trans.Visitor.Path" -- {{{
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
                            sendVisitorDownPath (Seq.singleton (CacheStep undefined :: VisitorStep)) (undefined `mplus` undefined :: Visitor [Int])
                        ) >>= (@?= Left PastVisitorIsInconsistentWithPresentVisitor)
                     -- }}}
                    ,testCase "choice step with cache" $ -- {{{
                        try (
                            evaluate
                            .
                            runVisitor
                            $
                            sendVisitorDownPath (Seq.singleton (ChoiceStep undefined :: VisitorStep)) (cache undefined :: Visitor [Int])
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
                            sendVisitorDownPath (Seq.singleton (undefined :: VisitorStep)) (mzero :: Visitor [Int])
                        ) >>= (@?= Left VisitorTerminatedBeforeEndOfWalk)
                     -- }}}
                    ,testCase "return" $ -- {{{
                        try (
                            evaluate
                            .
                            runVisitor
                            $
                            sendVisitorDownPath (Seq.singleton (undefined :: VisitorStep)) (return (undefined :: [Int]))
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
    ,testGroup "Control.Monad.Trans.Visitor.Supervisor" -- {{{
        [testCase "immediately abort" $ -- {{{
            runVisitorSupervisor bad_test_supervisor_actions abortSupervisor
            >>= (@?= (VisitorSupervisorResult (Left (VisitorProgress Unexplored ())) ([] :: [Int])))
         -- }}}
        ,testGroup "adding and removing workers" -- {{{
            [testCase "add one worker then abort" $ do -- {{{
                (maybe_workload_ref,actions) ← addAcceptOneWorkloadAction bad_test_supervisor_actions
                (runVisitorSupervisor actions $ do
                    addWorker ()
                    abortSupervisor
                 ) >>= (@?= (VisitorSupervisorResult (Left (VisitorProgress Unexplored ())) [()]))
                readIORef maybe_workload_ref >>= (@?= Just ((),entire_workload))
             -- }}}
            ,testCase "add then remove one worker then abort" $ do -- {{{
                (maybe_workload_ref,actions) ← addAcceptOneWorkloadAction bad_test_supervisor_actions
                (runVisitorSupervisor actions $ do
                    addWorker ()
                    removeWorker ()
                    abortSupervisor
                 ) >>= (@?= (VisitorSupervisorResult (Left (VisitorProgress Unexplored ())) []))
                readIORef maybe_workload_ref >>= (@?= Just ((),entire_workload)) 
             -- }}}
            ,testCase "add then remove then add one worker then abort" $ do -- {{{
                (maybe_workload_ref,actions) ← addAcceptMultipleWorkloadsAction bad_test_supervisor_actions
                (runVisitorSupervisor actions $ do
                    addWorker 1
                    removeWorker 1
                    addWorker 2
                    abortSupervisor
                 ) >>= (@?= (VisitorSupervisorResult (Left (VisitorProgress Unexplored ())) ([2::Int])))
                readIORef maybe_workload_ref >>= (@?= [(1,entire_workload),(2,entire_workload)]) 
             -- }}}
            ,testCase "add two workers then remove first worker then abort" $ do -- {{{
                (maybe_workload_ref,actions1) ← addAcceptMultipleWorkloadsAction bad_test_supervisor_actions
                (broadcast_ids_list_ref,actions2) ← addAppendWorkloadStealBroadcastIdsAction actions1
                (runVisitorSupervisor actions2 $ do
                    addWorker 1
                    addWorker 2
                    removeWorker 1
                    abortSupervisor
                 ) >>= (@?= (VisitorSupervisorResult (Left (VisitorProgress Unexplored ())) ([2::Int])))
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
                    VisitorSupervisorResult progress remaining_worker_ids ← runVisitorSupervisor actions_2 $ do
                        mapM_ addWorker worker_ids_to_add
                        mapM_ removeWorker worker_ids_to_remove
                        abortSupervisor
                    progress @?= Left (VisitorProgress Unexplored ())
                    worker_ids_left @?= sort remaining_worker_ids
                    readIORef maybe_workload_ref >>= (@?= Just (head worker_ids_to_add,entire_workload))
                    readIORef broadcast_ids_list_ref >>= (@?= if (null . tail) worker_ids_to_add then [] else [[head worker_ids_to_add]])
             -- }}}
            ]
         -- }}}
        ,testGroup "progress updates" -- {{{
            [testCase "request progress update when no workers present" $ do -- {{{
                (maybe_progress_ref,actions) ← addReceiveCurrentProgressAction bad_test_supervisor_actions
                (runVisitorSupervisor actions $ do
                    performGlobalProgressUpdate
                    abortSupervisor
                 ) >>= (@?= (VisitorSupervisorResult (Left (VisitorProgress Unexplored ())) ([]::[()])))
                readIORef maybe_progress_ref >>= (@?= Just (VisitorProgress Unexplored ()))
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
                    let progress = VisitorProgress Unexplored (Sum 0)
                    (runVisitorSupervisor actions3 $ do
                        addWorker 0
                        forM_ (tail active_workers) $ \worker_id → do
                            addWorker worker_id
                            receiveStolenWorkload 0 $ Just (VisitorWorkerStolenWorkload (VisitorWorkerProgressUpdate mempty entire_workload) undefined)
                        mapM_ addWorker inactive_workers
                        performGlobalProgressUpdate
                        mapM_ removeWorker active_workers
                        abortSupervisor
                     ) >>= (@?= (VisitorSupervisorResult (Left progress)) inactive_workers)
                    readIORef broadcast_ids_list_ref >>= (@?= [active_workers])
                    readIORef maybe_progress_ref >>= (@?= Just progress)
             -- }}}
            ,testCase "request and receive Just progress update when one worker present" $ do -- {{{
                (maybe_progress_ref,actions1) ← addReceiveCurrentProgressAction bad_test_supervisor_actions
                (broadcast_ids_list_ref,actions2) ← addAppendProgressBroadcastIdsAction actions1
                let actions3 = ignoreAcceptWorkloadAction actions2
                let progress = VisitorProgress (ChoiceCheckpoint Unexplored Unexplored) (Sum 1)
                (runVisitorSupervisor actions3 $ do
                    addWorker ()
                    performGlobalProgressUpdate
                    receiveProgressUpdate () $ VisitorWorkerProgressUpdate progress undefined
                    abortSupervisor
                 ) >>= (@?= (VisitorSupervisorResult (Left progress)) [()])
                readIORef maybe_progress_ref >>= (@?= Just progress)
                readIORef broadcast_ids_list_ref >>= (@?= [[()]])
             -- }}}
            ,testCase "request and receive progress update when active and inactive workers present" $ do -- {{{
                (maybe_progress_ref,actions1) ← addReceiveCurrentProgressAction bad_test_supervisor_actions
                (broadcast_ids_list_ref,actions2) ← addAppendProgressBroadcastIdsAction actions1
                let actions3 = ignoreAcceptWorkloadAction . ignoreWorkloadStealAction $ actions2
                let progress = VisitorProgress (ChoiceCheckpoint Unexplored Unexplored) (Sum 1)
                (runVisitorSupervisor actions3 $ do
                    addWorker (1 :: Int)
                    addWorker (2 :: Int)
                    performGlobalProgressUpdate
                    receiveProgressUpdate 1 $ VisitorWorkerProgressUpdate progress undefined
                    abortSupervisor
                 ) >>= (@?= (VisitorSupervisorResult (Left progress)) [1,2])
                readIORef maybe_progress_ref >>= (@?= Just progress)
                readIORef broadcast_ids_list_ref >>= (@?= [[1]])
             -- }}}
            ,testCase "final progress update ends the server monad" $ do -- {{{
                let actions = ignoreAcceptWorkloadAction bad_test_supervisor_actions
                let progress = VisitorProgress Explored (Sum 1)
                (runVisitorSupervisor actions $ do
                    addWorker ()
                    receiveProgressUpdate () $ VisitorWorkerProgressUpdate progress undefined
                    forever $
                        liftIO $
                            assertFailure "loop continued past final progress update"
                 ) >>= (@?= (VisitorSupervisorResult (Right (Sum 1)) [()]))
             -- }}}
            ]
         -- }}}
        ,testGroup "workload steals" -- {{{
            [testCase "failure to steal from a worker leads to second attempt" $ do -- {{{ 
                (broadcast_ids_list_ref,actions1) ← addAppendWorkloadStealBroadcastIdsAction bad_test_supervisor_actions
                let actions2 = ignoreAcceptWorkloadAction actions1
                (runVisitorSupervisor actions2 $ do
                    addWorker (1::Int)
                    addWorker 2
                    receiveStolenWorkload 1 Nothing
                    abortSupervisor
                 ) >>= (@?= (VisitorSupervisorResult (Left (VisitorProgress Unexplored ())) [1,2]))
                readIORef broadcast_ids_list_ref >>= (@?= [[1],[1]])
             -- }}}
            ]
         -- }}}
        ,testCase "starting from previous checkpoint" $ do -- {{{
            (maybe_workload_ref,actions) ← addAcceptOneWorkloadAction bad_test_supervisor_actions
            let checkpoint = ChoiceCheckpoint Unexplored Unexplored
                progress = VisitorProgress checkpoint (Sum 1)
            (runVisitorSupervisorStartingFrom progress actions $ do
                addWorker ()
                abortSupervisor
             ) >>= (@?= (VisitorSupervisorResult (Left progress) [()]))
            readIORef maybe_workload_ref >>= (@?= Just ((),(VisitorWorkload Seq.empty checkpoint)))
         -- }}}
        ]
     -- }}}
    ,testGroup "Control.Monad.Trans.Visitor.Worker" -- {{{
        [testGroup "forkVisitor(X)WorkerThread" -- {{{
            [testCase "abort" $ do -- {{{
                termination_result_ivar ← IVar.new
                semaphore ← newQSem 0
                VisitorWorkerEnvironment{..} ← forkVisitorIOWorkerThread
                    (IVar.write termination_result_ivar)
                    (liftIO (waitQSem semaphore) `mplus` error "should never get here")
                    entire_workload
                sendAbortRequest workerPendingRequests
                signalQSem semaphore
                termination_result ← IVar.blocking $ IVar.read termination_result_ivar
                case termination_result of
                    VisitorWorkerFinished _ → assertFailure "worker faled to abort"
                    VisitorWorkerFailed exception → assertFailure ("worker threw exception: " ++ show exception)
                    VisitorWorkerAborted → return ()
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
                    VisitorProgress checkpoint solutions ←
                        (IVar.blocking $ IVar.read solutions_ivar)
                        >>=
                        \termination_reason → case termination_reason of
                            VisitorWorkerFinished final_progress → return final_progress
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
                            (VisitorWorkload path Unexplored)
                    VisitorProgress checkpoint solutions ←
                        (IVar.blocking $ IVar.read solutions_ivar)
                        >>=
                        \termination_reason → case termination_reason of
                            VisitorWorkerFinished final_progress → return final_progress
                            other → error ("terminated unsuccessfully with reason " ++ show other)
                    checkpoint @?= checkpointFromInitialPath path Explored
                    solutions @?= (runVisitor . sendVisitorDownPath path $ visitor)
                    return True
                 -- }}}
                ]
             -- }}}
            ,testProperty "progress updates correctly capture current and remaining progress" $ \(visitor :: Visitor [Int]) → unsafePerformIO $ do -- {{{
                termination_result_ivar ← IVar.new
                (startWorker,VisitorWorkerEnvironment{..}) ← preforkVisitorWorkerThread
                    (IVar.write termination_result_ivar)
                    visitor
                    entire_workload
                progress_updates_ref ← newIORef DList.empty
                let sendMyProgressUpdateRequest = sendProgressUpdateRequest workerPendingRequests submitProgressUpdate
                    submitProgressUpdate progress_update = do
                        atomicModifyIORef progress_updates_ref $ (,()) . flip DList.snoc progress_update
                        sendMyProgressUpdateRequest
                sendMyProgressUpdateRequest
                startWorker
                termination_result ← IVar.blocking $ IVar.read termination_result_ivar
                remaining_solutions ← case termination_result of
                    VisitorWorkerFinished (visitorResult → solutions) → return solutions
                    VisitorWorkerFailed exception → error ("worker threw exception: " ++ show exception)
                    VisitorWorkerAborted → error "worker aborted prematurely"
                (IVar.nonblocking . IVar.read) workerTerminationFlag >>= assertBool "is the termination flag set?" . isJust
                progress_updates ← fmap DList.toList (readIORef progress_updates_ref)
                let correct_solutions = runVisitor visitor
                correct_solutions @=? ((⊕ remaining_solutions) . mconcat . fmap (visitorResult . visitorWorkerProgressUpdate) $ progress_updates)
                let results_using_progressive_checkpoints =
                        zipWith
                            mappend
                            (scanl1 mappend $ map (visitorResult . visitorWorkerProgressUpdate) progress_updates)
                            (map (flip runVisitorThroughCheckpoint visitor . visitorCheckpoint . visitorWorkerProgressUpdate) progress_updates)
                assertBool "Do runs starting from the progress update checkpoints get the correct result?" $
                    all (== correct_solutions) results_using_progressive_checkpoints
                let results_using_progressive_workloads =
                        zipWith
                            mappend
                            (scanl1 mappend $ map (visitorResult . visitorWorkerProgressUpdate) progress_updates)
                            (map (flip runVisitorThroughWorkload visitor . visitorWorkerRemainingWorkload) progress_updates)
                assertBool "Do runs starting from the progress update workloads get the correct result?" $
                    all (== correct_solutions) results_using_progressive_workloads
                return True
             -- }}}
            ,testCase "terminates successfully with null visitor" $ do -- {{{
                termination_result_ivar ← IVar.new
                VisitorWorkerEnvironment{..} ←
                    forkVisitorWorkerThread
                        (IVar.write termination_result_ivar)
                        (mzero :: Visitor [Int])
                        entire_workload
                termination_result ← IVar.blocking $ IVar.read termination_result_ivar
                case termination_result of
                    VisitorWorkerFinished (visitorResult → solutions) → solutions @?= mempty
                    VisitorWorkerFailed exception → assertFailure ("worker threw exception: " ++ show exception)
                    VisitorWorkerAborted → assertFailure "worker prematurely aborted"
                workerInitialPath @?= Seq.empty
                (IVar.nonblocking . IVar.read) workerTerminationFlag >>= assertBool "is the termination flag set?" . isJust
             -- }}}
            ,testGroup "work stealing correctly preserves total workload" -- {{{
                [testProperty "single steal" $ flip fmap arbitrary $ \(visitor :: VisitorIO (Set Int)) → unsafePerformIO $ do -- {{{
                    worker_started_qsem ← newQSem 0
                    blocking_value_ivar ← IVar.new
                    let visitor_with_blocking_value =
                            ((liftIO (do
                                signalQSem worker_started_qsem
                                IVar.blocking . IVar.read $ blocking_value_ivar
                            ))
                            ⊕ return (Set.singleton 24))
                            ⊕ visitor
                    termination_result_ivar ← IVar.new
                    VisitorWorkerEnvironment{..} ← forkVisitorIOWorkerThread
                        (IVar.write termination_result_ivar)
                        visitor_with_blocking_value
                        entire_workload
                    maybe_workload_ref ← newIORef Nothing
                    waitQSem worker_started_qsem
                    sendWorkloadStealRequest workerPendingRequests $ writeIORef maybe_workload_ref
                    IVar.write blocking_value_ivar (Set.singleton 42)
                    VisitorProgress checkpoint remaining_solutions ←
                        (IVar.blocking $ IVar.read termination_result_ivar)
                        >>=
                        \termination_result → case termination_result of
                            VisitorWorkerFinished final_progress → return final_progress
                            VisitorWorkerFailed exception → error ("worker threw exception: " ++ show exception)
                            VisitorWorkerAborted → error "worker aborted prematurely"
                    (IVar.nonblocking . IVar.read) workerTerminationFlag >>= assertBool "is the termination flag set?" . isJust
                    VisitorWorkerStolenWorkload (VisitorWorkerProgressUpdate (VisitorProgress checkpoint prestolen_solutions) remaining_workload) workload ←
                        fmap (fromMaybe (error "stolen workload not available"))
                        $
                        readIORef maybe_workload_ref
                    assertBool "Does the checkpoint have unexplored nodes?" $ mergeAllCheckpointNodes checkpoint /= Explored
                    runVisitorTThroughWorkload remaining_workload visitor_with_blocking_value >>= (remaining_solutions @?=)
                    correct_solutions ← runVisitorT visitor_with_blocking_value
                    stolen_solutions_via_workload ← runVisitorTThroughWorkload workload visitor_with_blocking_value
                    correct_solutions @=? mconcat [prestolen_solutions,remaining_solutions,stolen_solutions_via_workload]
                    stolen_solutions_via_checkpoint ← runVisitorTThroughCheckpoint checkpoint visitor_with_blocking_value
                    correct_solutions @=? mconcat [prestolen_solutions,remaining_solutions,stolen_solutions_via_checkpoint]
                    stolen_solutions_via_remaining_workload ← runVisitorTThroughWorkload remaining_workload visitor_with_blocking_value
                    correct_solutions @=? mconcat [prestolen_solutions,remaining_solutions,stolen_solutions_via_remaining_workload]
                    return True
                 -- }}}
                ,testProperty "continuous stealing" $ \(visitor :: Visitor (Set Int)) → unsafePerformIO $ do -- {{{
                    termination_result_ivar ← IVar.new
                    (startWorker,VisitorWorkerEnvironment{..}) ← preforkVisitorWorkerThread
                        (IVar.write termination_result_ivar)
                        visitor
                        entire_workload
                    workloads_ref ← newIORef DList.empty
                    let submitMyWorkloadStealReqest = sendWorkloadStealRequest workerPendingRequests submitStolenWorkload
                        submitStolenWorkload workload = do
                            atomicModifyIORef workloads_ref $ (,()) . flip DList.snoc workload
                            submitMyWorkloadStealReqest
                    submitMyWorkloadStealReqest
                    startWorker
                    termination_result ← IVar.blocking $ IVar.read termination_result_ivar
                    remaining_solutions ← case termination_result of
                        VisitorWorkerFinished (visitorResult → solutions) → return solutions
                        VisitorWorkerFailed exception → error ("worker threw exception: " ++ show exception)
                        VisitorWorkerAborted → error "worker aborted prematurely"
                    steals ← fmap (catMaybes . DList.toList) (readIORef workloads_ref)
                    let correct_solutions = runVisitor visitor
                    correct_solutions @=?
                        (remaining_solutions ⊕ (mconcat . map (flip runVisitorThroughWorkload visitor. visitorWorkerStolenWorkload) $ steals))
                    assertBool "Do the progress update workloads get the correct answer?" $
                        (all (== correct_solutions))
                        (zipWith
                            mappend
                            (scanl1 mappend $ map (visitorResult . visitorWorkerProgressUpdate . visitorWorkerStolenWorkerProgressUpdate) steals)
                            (map (flip runVisitorThroughWorkload visitor . visitorWorkerRemainingWorkload . visitorWorkerStolenWorkerProgressUpdate) steals)
                        )
                    assertBool "Do the progress update checkpoints get the correct answer?" $
                        (all (== correct_solutions))
                        (zipWith
                            mappend
                            (scanl1 mappend $ map (visitorResult . visitorWorkerProgressUpdate . visitorWorkerStolenWorkerProgressUpdate) steals)
                            (map (flip runVisitorThroughCheckpoint visitor . visitorCheckpoint . visitorWorkerProgressUpdate . visitorWorkerStolenWorkerProgressUpdate) steals)
                        )
                    return True
                 -- }}}
                ]
             -- }}}
            ]
         -- }}}
        ]
     -- }}}
    ]
-- }}}
