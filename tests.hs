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
import qualified Data.IVar as IVar
import Data.List (mapAccumL,sort)
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
import Test.QuickCheck.Property

import Control.Monad.Trans.Visitor
import Control.Monad.Trans.Visitor.Checkpoint
import Control.Monad.Trans.Visitor.Label
import Control.Monad.Trans.Visitor.Path
import Control.Monad.Trans.Visitor.Workload
import Control.Monad.Trans.Visitor.Worker
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

instance (Serialize α, Arbitrary α, Functor m, Monad m) ⇒ Arbitrary (VisitorT m α) where -- {{{
    arbitrary = sized arb
      where
        arb :: Int → Gen (VisitorT m α)
        arb 0 = frequency
                    [(3,result)
                    ,(1,null)
                    ,(2,cached)
                    ]
        arb n = frequency
                    [(3,result)
                    ,(2,bindToArbitrary n result)
                    ,(1,bindToArbitrary n null)
                    ,(2,bindToArbitrary n cached)
                    ,(4,liftM2 mplus (arb (n `div` 2)) (arb (n `div` 2)))
                    ]
        null, result, cached :: Gen (VisitorT m α)
        null = return mzero
        result = fmap return arbitrary
        cached = fmap cache arbitrary

        bindToArbitrary :: Int → Gen (VisitorT m α) → Gen (VisitorT m α)
        bindToArbitrary n = flip (liftM2 (>>)) (arb (n-1))
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

instance Arbitrary α ⇒ Arbitrary (VisitorStatusUpdate α) where -- {{{
    arbitrary = VisitorStatusUpdate <$> arbitrary <*> arbitrary
-- }}}

instance Arbitrary α ⇒ Arbitrary (VisitorWorkerStatusUpdate α) where -- {{{
    arbitrary = VisitorWorkerStatusUpdate <$> arbitrary <*> arbitrary
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
echo :: Show α ⇒ α → α -- {{{
echo x = trace (show x) x
-- }}}

echoWithLabel :: Show α ⇒ String → α → α -- {{{
echoWithLabel label x = trace (label ++ " " ++ show x) x
-- }}} 
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

-- }}} Helpers

main = defaultMain tests

tests = -- {{{
    [testGroup "Control.Monad.Trans.Visitor" -- {{{
        [testGroup "Eq instance" -- {{{
            [testProperty "self" $ \(v :: Visitor Int) → v == v
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
                ) @?= ((),[1,2,3])
             -- }}}
            ]
         -- }}}
        ,testGroup "runVisitorTAndGatherResults" -- {{{
            [testCase "Writer" $ -- {{{
                (runWriter . runVisitorTAndGatherResults $ do
                    cache [1 :: Int] >>= lift . tell
                    (lift (tell [2]) `mplus` lift (tell [3]))
                    return [42]
                ) @?= ([42,42],[1,2,3])
             -- }}}
            ]
         -- }}}
        ]
     -- }}}
    ,testGroup "Control.Monad.Trans.Visitor.Checkpoint" -- {{{
        [testGroup "contextFromCheckpoint" -- {{{
            [testProperty "branch" $ \(checkpoint :: VisitorCheckpoint) (active_branch :: Branch) → -- {{{
                checkpointFromContext (Seq.singleton (BranchContextStep active_branch)) checkpoint
                ==
                (mergeCheckpointRoot $ case active_branch of
                    LeftBranch → ChoiceCheckpoint checkpoint Explored
                    RightBranch → ChoiceCheckpoint Explored checkpoint)
             -- }}}
            ,testProperty "cache" $ \(checkpoint :: VisitorCheckpoint) (i :: Int) → -- {{{
                checkpointFromContext (Seq.singleton (CacheContextStep (encode i))) checkpoint
                ==
                (mergeCheckpointRoot $ CacheCheckpoint (encode i) checkpoint)
             -- }}}
            ,testProperty "choice" $ \(inner_checkpoint :: VisitorCheckpoint) (other_visitor :: Visitor Int) (other_checkpoint :: VisitorCheckpoint) → -- {{{
                (checkpointFromContext (Seq.singleton (LeftChoiceContextStep other_checkpoint other_visitor)) inner_checkpoint)
                ==
                (mergeCheckpointRoot $ ChoiceCheckpoint inner_checkpoint other_checkpoint)
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
                    solutions1 = runVisitorThroughCheckpointAndGatherResults checkpoint1 visitor
                    solutions2 = runVisitorThroughCheckpointAndGatherResults checkpoint2 visitor
                    solutions3 = runVisitorThroughCheckpointAndGatherResults checkpoint3 visitor
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
        ,testGroup "runVisitorThroughCheckpoint" -- {{{
            [testProperty "matches walk down path" $ \(visitor :: Visitor [Int]) → randomPathForVisitor visitor >>= \path → return $ -- {{{
                runVisitor (walkVisitorDownPath path visitor)
                ==
                (fst . last) (runVisitorThroughCheckpoint (checkpointFromUnexploredPath path) visitor)
             -- }}}
            ]
         -- }}}
        ,testGroup "runVisitorWithCheckpoints" -- {{{
            [testProperty "last checkpoint is correct" $ \(v :: Visitor ()) → -- {{{
                let checkpoints = runVisitorWithCheckpoints v
                in unsafePerformIO $ (last checkpoints @=? (runVisitor v,Explored)) >> return True
             -- }}}
            ,testProperty "checkpoints accurately capture remaining search space" $ \(v :: Visitor [Int]) → -- {{{
                let results_using_progressive_checkpoints =
                        [result ⊕ runVisitorThroughCheckpointAndGatherResults checkpoint v
                        | (result,checkpoint) ← runVisitorWithCheckpoints v
                        ]
                in all (== head results_using_progressive_checkpoints) (tail results_using_progressive_checkpoints)
             -- }}}
            ,testGroup "example instances" -- {{{
                [testGroup "mplus" -- {{{
                    [testCase "mzero + mzero" $ -- {{{
                        runVisitorWithCheckpoints (mzero `mplus` mzero :: Visitor (Maybe ()))
                        @?=
                        [(Nothing,Unexplored)
                        ,(Nothing,ChoiceCheckpoint Explored Unexplored)
                        ,(Nothing,Explored)
                        ]
                     -- }}}
                    ,testCase "mzero + return" $ -- {{{
                        runVisitorWithCheckpoints (mzero `mplus` return (Just ()) :: Visitor (Maybe ()))
                        @?=
                        [(Nothing,Unexplored)
                        ,(Nothing,ChoiceCheckpoint Explored Unexplored)
                        ,(Just (),Explored)
                        ]
                     -- }}}
                    ,testCase "return + mzero" $ -- {{{
                        runVisitorWithCheckpoints (return (Just ()) `mplus` mzero :: Visitor (Maybe ()))
                        @?=
                        [(Nothing,Unexplored)
                        ,(Just (),ChoiceCheckpoint Explored Unexplored)
                        ,(Just (),Explored)
                        ]
                     -- }}}
                    ]
                 -- }}}
                ,testCase "mzero" $ runVisitorWithCheckpoints (mzero :: Visitor [Int]) @?= [([],Explored)]
                ,testCase "return" $ runVisitorWithCheckpoints (return [0] :: Visitor [Int]) @?= [([0],Explored)]
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
            [testProperty "same result as runVisitor" $ \(visitor :: Visitor Int) →
                 runVisitor ((:[]) <$> visitor) == (visitorSolutionResult <$> runVisitorWithLabels visitor)
            ]
         -- }}}
        ,testGroup "walkVisitorDownLabel" -- {{{
            [testProperty "same result as walking down path" $ do -- {{{
                visitor :: Visitor Int ← randomVisitorWithoutCache
                path ← randomPathForVisitor visitor
                let label = labelFromPath path
                return $
                    walkVisitorDownPath path visitor
                    ==
                    walkVisitorDownLabel label visitor
             -- }}}
            ]
         -- }}}
        ]
     -- }}}
    ,testGroup "Control.Monad.Trans.Visitor.Path" -- {{{
        [testGroup "walkVisitorDownPath" -- {{{
            [testCase "null path" $ (runVisitor . walkVisitorDownPath Seq.empty) (return [42]) @?= [42]
            ,testCase "cache" $ do (runVisitor . walkVisitorDownPath (Seq.singleton (CacheStep (encode ([42 :: Int]))))) (cache (undefined :: [Int])) @?= [42]
            ,testCase "cacheGuard" $ do (runVisitor . walkVisitorDownPath (Seq.singleton (CacheStep (encode ())))) (cacheGuard False >> return [42::Int]) @?= [42]
            ,testCase "choice" $ do -- {{{
                (runVisitor . walkVisitorDownPath (Seq.singleton (ChoiceStep LeftBranch))) (return [42] `mplus` undefined) @?= [42]
                (runVisitor . walkVisitorDownPath (Seq.singleton (ChoiceStep RightBranch))) (undefined `mplus` return [42]) @?= [42]
             -- }}}
            ,testGroup "errors" -- {{{
                [testGroup "PastVisitorIsInconsistentWithPresentVisitor" -- {{{
                    [testCase "cache step with choice" $ -- {{{
                        try (
                            evaluate
                            .
                            runVisitor
                            $
                            walkVisitorDownPath (Seq.singleton (CacheStep undefined :: VisitorStep)) (undefined `mplus` undefined :: Visitor [Int])
                        ) >>= (@?= Left PastVisitorIsInconsistentWithPresentVisitor)
                     -- }}}
                    ,testCase "choice step with cache" $ -- {{{
                        try (
                            evaluate
                            .
                            runVisitor
                            $
                            walkVisitorDownPath (Seq.singleton (ChoiceStep undefined :: VisitorStep)) (cache undefined :: Visitor [Int])
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
                            walkVisitorDownPath (Seq.singleton (undefined :: VisitorStep)) (mzero :: Visitor [Int])
                        ) >>= (@?= Left VisitorTerminatedBeforeEndOfWalk)
                     -- }}}
                    ,testCase "return" $ -- {{{
                        try (
                            evaluate
                            .
                            runVisitor
                            $
                            walkVisitorDownPath (Seq.singleton (undefined :: VisitorStep)) (return (undefined :: [Int]))
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
                        runWriter . walkVisitorTDownPath (Seq.singleton (CacheStep . encode $ [24 :: Int])) $ do
                            runAndCache (tell [1] >> return [42 :: Int] :: Writer [Int] [Int])
                log @?= []
                (runWriter . runVisitorTAndGatherResults $ transformed_visitor) @?= ([24],[])
             -- }}}
            ,testCase "choice step" $ do -- {{{
                let (transformed_visitor,log) =
                        runWriter . walkVisitorTDownPath (Seq.singleton (ChoiceStep RightBranch)) $ do
                            lift (tell [1])
                            (lift (tell [2]) `mplus` lift (tell [3]))
                            lift (tell [4])
                            return [42]
                log @?= [1]
                (runWriter . runVisitorTAndGatherResults $ transformed_visitor) @?= ([42],[3,4])
             -- }}}
            ]
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
                writeIORef workerPendingRequests Nothing
                signalQSem semaphore
                termination_result ← IVar.blocking $ IVar.read termination_result_ivar
                case termination_result of
                    VisitorWorkerFinished _ → assertFailure "worker faled to abort"
                    VisitorWorkerFailed exception → assertFailure ("worker threw exception: " ++ show exception)
                    VisitorWorkerAborted → return ()
                workerInitialPath @?= Seq.empty
                readIORef workerPendingRequests >>= assertBool "is the request queue still null?" . isNothing
             -- }}}
            ,testGroup "obtains all solutions" -- {{{
                [testProperty "with no initial path" $ \(visitor :: Visitor [Int]) → unsafePerformIO $ do -- {{{
                    solutions_ivar ← IVar.new
                    worker_environment ←
                        forkVisitorWorkerThread
                            (IVar.write solutions_ivar)
                            visitor
                            entire_workload
                    VisitorStatusUpdate checkpoint solutions ←
                        (IVar.blocking $ IVar.read solutions_ivar)
                        >>=
                        \termination_reason → case termination_reason of
                            VisitorWorkerFinished final_status_update → return final_status_update
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
                    VisitorStatusUpdate checkpoint solutions ←
                        (IVar.blocking $ IVar.read solutions_ivar)
                        >>=
                        \termination_reason → case termination_reason of
                            VisitorWorkerFinished final_status_update → return final_status_update
                            other → error ("terminated unsuccessfully with reason " ++ show other)
                    checkpoint @?= checkpointFromInitialPath path Explored
                    solutions @?= (runVisitor . walkVisitorDownPath path $ visitor)
                    return True
                 -- }}}
                ]
             -- }}}
            ,testProperty "status updates produce valid checkpoints" $ \(visitor :: Visitor [Int]) → unsafePerformIO $ do -- {{{
                termination_result_ivar ← IVar.new
                (startWorker,VisitorWorkerEnvironment{..}) ← preforkVisitorWorkerThread
                    (IVar.write termination_result_ivar)
                    visitor
                    entire_workload
                checkpoints_ref ← newIORef DList.empty
                let status_update_requests = Seq.singleton . StatusUpdateRequested $
                        maybe
                            (atomicModifyIORef workerPendingRequests $ (,()) . fmap (const Seq.empty))
                            (\checkpoint → do
                                atomicModifyIORef checkpoints_ref $ (,()) . flip DList.snoc checkpoint
                                atomicModifyIORef workerPendingRequests $ (,()) . fmap (const status_update_requests)
                            )
                writeIORef workerPendingRequests . Just $ status_update_requests
                startWorker
                termination_result ← IVar.blocking $ IVar.read termination_result_ivar
                remaining_solutions ← case termination_result of
                    VisitorWorkerFinished (visitorStatusNewResults → solutions) → return solutions
                    VisitorWorkerFailed exception → error ("worker threw exception: " ++ show exception)
                    VisitorWorkerAborted → error "worker aborted prematurely"
                readIORef workerPendingRequests >>= assertBool "has the request queue been nulled?" . isNothing
                checkpoints ← fmap DList.toList (readIORef checkpoints_ref)
                let correct_solutions = runVisitor visitor
                correct_solutions @=? ((⊕ remaining_solutions) . mconcat . fmap (visitorStatusNewResults . visitorWorkerStatusUpdate) $ checkpoints)
                let results_using_progressive_checkpoints =
                        zipWith
                            mappend
                            (scanl1 mappend $ map (visitorStatusNewResults . visitorWorkerStatusUpdate) checkpoints)
                            (map (flip runVisitorThroughCheckpointAndGatherResults visitor . visitorStatusCheckpoint . visitorWorkerStatusUpdate) checkpoints)
                return $ all (== head results_using_progressive_checkpoints) (tail results_using_progressive_checkpoints)
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
                    VisitorWorkerFinished (visitorStatusNewResults → solutions) → solutions @?= mempty
                    VisitorWorkerFailed exception → assertFailure ("worker threw exception: " ++ show exception)
                    VisitorWorkerAborted → assertFailure "worker prematurely aborted"
                workerInitialPath @?= Seq.empty
                readIORef workerPendingRequests >>= assertBool "has the request queue been nulled?" . isNothing
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
                    maybe_maybe_workload_ref ← newIORef Nothing
                    waitQSem worker_started_qsem
                    writeIORef workerPendingRequests . Just . Seq.singleton . WorkloadStealRequested $ writeIORef maybe_maybe_workload_ref . Just
                    IVar.write blocking_value_ivar (Set.singleton 42)
                    VisitorStatusUpdate checkpoint remaining_solutions ←
                        (IVar.blocking $ IVar.read termination_result_ivar)
                        >>=
                        \termination_result → case termination_result of
                            VisitorWorkerFinished final_status_update → return final_status_update
                            VisitorWorkerFailed exception → error ("worker threw exception: " ++ show exception)
                            VisitorWorkerAborted → error "worker aborted prematurely"
                    readIORef workerPendingRequests >>= assertBool "has the request queue been nulled?" . isNothing
                    VisitorWorkerStolenWorkload (VisitorWorkerStatusUpdate (VisitorStatusUpdate checkpoint prestolen_solutions) remaining_workload) workload ←
                        fmap (
                            fromMaybe (error "stolen workload not available")
                            .
                            fromMaybe (error "steal request ignored")
                        )
                        $
                        readIORef maybe_maybe_workload_ref
                    assertBool "Does the checkpoint have unexplored nodes?" $ mergeAllCheckpointNodes checkpoint /= Explored
                    runVisitorTThroughWorkloadAndGatherResults remaining_workload visitor_with_blocking_value >>= (remaining_solutions @?=)
                    stolen_solutions ← runVisitorTThroughWorkloadAndGatherResults workload visitor_with_blocking_value
                    let solutions = mconcat [prestolen_solutions,remaining_solutions,stolen_solutions]
                    correct_solutions ← runVisitorTAndGatherResults visitor_with_blocking_value
                    solutions @?= correct_solutions
                    return True
                 -- }}}
                ,testProperty "continuous stealing" $ \(visitor :: Visitor (Set Int)) → unsafePerformIO $ do -- {{{
                    termination_result_ivar ← IVar.new
                    (startWorker,VisitorWorkerEnvironment{..}) ← preforkVisitorWorkerThread
                        (IVar.write termination_result_ivar)
                        visitor
                        entire_workload
                    workloads_ref ← newIORef DList.empty
                    let submitWorkloadStealReqest = atomicModifyIORef workerPendingRequests $ (,()) . fmap (const workload_steal_requests)
                        workload_steal_requests = Seq.singleton . WorkloadStealRequested $
                            maybe
                                submitWorkloadStealReqest
                                (\workload → do
                                    atomicModifyIORef workloads_ref $ (,()) . flip DList.snoc workload
                                    submitWorkloadStealReqest
                                )
                    writeIORef workerPendingRequests . Just $ workload_steal_requests
                    startWorker
                    termination_result ← IVar.blocking $ IVar.read termination_result_ivar
                    remaining_solutions ← case termination_result of
                        VisitorWorkerFinished (visitorStatusNewResults → solutions) → return solutions
                        VisitorWorkerFailed exception → error ("worker threw exception: " ++ show exception)
                        VisitorWorkerAborted → error "worker aborted prematurely"
                    workloads ← fmap (map visitorWorkerStolenWorkload . DList.toList) (readIORef workloads_ref)
                    let stolen_solutions = mconcat . map (flip runVisitorThroughWorkloadAndGatherResults visitor) $ workloads
                        solutions = remaining_solutions ⊕ stolen_solutions
                        correct_solutions = runVisitor visitor
                    solutions @?= correct_solutions
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
