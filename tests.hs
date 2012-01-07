-- Language extensions {{{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- }}}

-- Imports {{{
import Control.Applicative (liftA2)
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
import qualified Data.UUID as UUID
import Data.UUID (UUID)

import Debug.Trace (trace)

import Reactive.Banana
import qualified Reactive.Banana.Model as Model
import qualified Reactive.Banana.Implementation as PushIO

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
import Control.Monad.Trans.Visitor.Reactive
import Control.Monad.Trans.Visitor.Reactive.Supervisor
import Control.Monad.Trans.Visitor.Reactive.Worker
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

-- Values {{{
empty_worker_incoming_events = -- {{{
    VisitorWorkerIncomingEvents
        never
        never
        never
-- }}}
-- }}} Values

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

interpretSupervisorUsingModel :: -- {{{
    ∀ α worker_id. (Ord worker_id, Show worker_id, Monoid α, Eq α) ⇒
    [VisitorSupervisorIncomingEvent worker_id α] →
    [[VisitorSupervisorOutgoingEvent worker_id α]]
interpretSupervisorUsingModel = Model.interpret mapSupervisorEvents
-- }}}

interpretSupervisorUsingPushIO :: -- {{{
    ∀ α worker_id. (Ord worker_id, Show worker_id, Monoid α, Eq α) ⇒
    [VisitorSupervisorIncomingEvent worker_id α] →
    IO [[VisitorSupervisorOutgoingEvent worker_id α]]
interpretSupervisorUsingPushIO = PushIO.interpret mapSupervisorEvents
-- }}}

mapSupervisorEvents :: -- {{{
    ∀ α ξ worker_id. (FRP ξ, Ord worker_id, Show worker_id, Monoid α, Eq α) ⇒
    Model.Event ξ (VisitorSupervisorIncomingEvent worker_id α) →
    Model.Event ξ (VisitorSupervisorOutgoingEvent worker_id α)
mapSupervisorEvents incoming = outgoing
  where
    VisitorSupervisorOutgoingEvents{..} = createVisitorSupervisorReactiveNetwork VisitorSupervisorIncomingEvents{..}

    visitorSupervisorIncomingWorkerAddedEvent =
        (\incoming_event →
            case incoming_event of
                VisitorSupervisorIncomingWorkerAddedEvent x → Just x
                _ → Nothing
        ) <$?> incoming

    visitorSupervisorIncomingWorkerRemovedEvent =
        (\incoming_event →
            case incoming_event of
                VisitorSupervisorIncomingWorkerRemovedEvent x → Just x
                _ → Nothing
        ) <$?> incoming

    visitorSupervisorIncomingWorkerStatusUpdateEvent =
        (\incoming_event →
            case incoming_event of
                VisitorSupervisorIncomingWorkerStatusUpdateEvent x → Just x
                _ → Nothing
        ) <$?> incoming

    visitorSupervisorIncomingWorkerWorkloadStolenEvent =
        (\incoming_event →
            case incoming_event of
                VisitorSupervisorIncomingWorkerWorkloadStolenEvent x → Just x
                _ → Nothing
        ) <$?> incoming

    visitorSupervisorIncomingWorkerFinishedEvent =
        (\incoming_event →
            case incoming_event of
                VisitorSupervisorIncomingWorkerFinishedEvent x → Just x
                _ → Nothing
        ) <$?> incoming

    visitorSupervisorIncomingRequestFullCheckpointEvent =
        (\incoming_event →
            case incoming_event of
                VisitorSupervisorIncomingRequestFullCheckpointEvent → Just ()
                _ → Nothing
        ) <$?> incoming

    readVisitorSupervisorCurrentStatus =
        (\incoming_event →
            case incoming_event of
                ReadVisitorSupervisorCurrentStatus → Just ()
                _ → Nothing
        ) <$?> incoming

    outgoing :: Model.Event ξ (VisitorSupervisorOutgoingEvent worker_id α)
    outgoing = mconcat
        [VisitorSupervisorOutgoingWorkloadEvent <$> visitorSupervisorOutgoingWorkloadEvent
        ,VisitorSupervisorOutgoingFinishedEvent <$> visitorSupervisorOutgoingFinishedEvent
        ,VisitorSupervisorOutgoingBroadcastWorkerRequestEvent <$> visitorSupervisorOutgoingBroadcastWorkerRequestEvent
        ,VisitorSupervisorOutgoingCheckpointCompleteEvent <$> visitorSupervisorOutgoingCheckpointCompleteEvent
        ,VisitorSupervisorOutgoingNewResultsFoundEvent <$> visitorSupervisorOutgoingNewResultsFoundEvent
        ,ResultVisitorSupervisorCurrentStatus <$> visitorSupervisorCurrentStatus <@ readVisitorSupervisorCurrentStatus
        ]
-- }}}

newSolutionsEventsFromStatusUpdate VisitorStatusUpdate{..} -- {{{
  | visitorStatusNewResults == mempty = []
  | otherwise = [VisitorSupervisorOutgoingNewResultsFoundEvent visitorStatusNewResults]
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

-- Types {{{
data VisitorSupervisorIncomingEvent worker_id α = -- {{{
    VisitorSupervisorIncomingWorkerAddedEvent worker_id
  | VisitorSupervisorIncomingWorkerRemovedEvent worker_id
  | VisitorSupervisorIncomingWorkerStatusUpdateEvent (WorkerIdTagged worker_id (VisitorWorkerStatusUpdate α))
  | VisitorSupervisorIncomingWorkerWorkloadStolenEvent (WorkerIdTagged worker_id (Maybe (VisitorWorkerStolenWorkload α)))
  | VisitorSupervisorIncomingWorkerFinishedEvent (WorkerIdTagged worker_id (VisitorStatusUpdate α))
  | VisitorSupervisorIncomingRequestFullCheckpointEvent
  | VisitorSupervisorIncomingNullEvent
  | ReadVisitorSupervisorCurrentStatus
  | ReadVisitorSupervisorCurrentRecruitedWorkers
  deriving (Show)
-- }}}

data VisitorSupervisorOutgoingEvent worker_id α = -- {{{
    VisitorSupervisorOutgoingWorkloadEvent (WorkerIdTagged worker_id VisitorWorkload)
  | VisitorSupervisorOutgoingFinishedEvent VisitorCheckpoint
  | VisitorSupervisorOutgoingBroadcastWorkerRequestEvent ([worker_id],VisitorWorkerReactiveRequest)
  | VisitorSupervisorOutgoingCheckpointCompleteEvent (VisitorStatusUpdate α)
  | VisitorSupervisorOutgoingNewResultsFoundEvent α
  | ResultVisitorSupervisorCurrentStatus (VisitorStatusUpdate α)
  deriving (Eq,Show)
-- }}}
-- }}} Types
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
    ,testGroup "Control.Monad.Trans.Visitor.Reactive.Supervisor" -- {{{
        [testGroup "createVisitorSupervisorReactiveNetwork" -- {{{
            [testCase "current checkpoint results in mempty" $ do -- {{{
                [[]] @=? interpretSupervisorUsingModel ([VisitorSupervisorIncomingNullEvent :: VisitorSupervisorIncomingEvent () ()])
             -- }}}
            ,testGroup "basic recruitment events" -- {{{
                [testCase "worker recruited" $ -- {{{
                    let incoming :: [VisitorSupervisorIncomingEvent () ()]
                        incoming =
                            [VisitorSupervisorIncomingWorkerAddedEvent ()
                            ]
                        correct_outgoing :: [[VisitorSupervisorOutgoingEvent () ()]]
                        correct_outgoing =
                            [[VisitorSupervisorOutgoingWorkloadEvent (WorkerIdTagged () entire_workload)]
                            ]
                    in correct_outgoing @=? interpretSupervisorUsingModel incoming
                 -- }}}
                ,testCase "worker recruited, then shuts down" $ -- {{{
                    let incoming :: [VisitorSupervisorIncomingEvent () ()]
                        incoming =
                            [VisitorSupervisorIncomingWorkerAddedEvent ()
                            ,VisitorSupervisorIncomingWorkerRemovedEvent ()
                            ]
                        correct_outgoing :: [[VisitorSupervisorOutgoingEvent () ()]]
                        correct_outgoing =
                            [[VisitorSupervisorOutgoingWorkloadEvent (WorkerIdTagged () entire_workload)]
                            ,[]
                            ]
                    in correct_outgoing @=? interpretSupervisorUsingModel incoming
                 -- }}}
                ,testCase "worker recruited, then shuts down, then is recruited again" $ -- {{{
                    let incoming :: [VisitorSupervisorIncomingEvent () ()]
                        incoming =
                            [VisitorSupervisorIncomingWorkerAddedEvent ()
                            ,VisitorSupervisorIncomingWorkerRemovedEvent ()
                            ,VisitorSupervisorIncomingWorkerAddedEvent ()
                            ]
                        correct_outgoing :: [[VisitorSupervisorOutgoingEvent () ()]]
                        correct_outgoing =
                            [[VisitorSupervisorOutgoingWorkloadEvent (WorkerIdTagged () entire_workload)]
                            ,[]
                            ,[VisitorSupervisorOutgoingWorkloadEvent (WorkerIdTagged () entire_workload)]
                            ]
                    in correct_outgoing @=? interpretSupervisorUsingModel incoming
                 -- }}}
                ]
             -- }}}
            ,testGroup "basic checkpointing events" -- {{{
                [testProperty "recruitment, update, current checkpoint" $ -- {{{
                  \(worker_status_update@(VisitorWorkerStatusUpdate status_update workload) :: VisitorWorkerStatusUpdate [Int]) → unsafePerformIO $
                    let incoming :: [VisitorSupervisorIncomingEvent () [Int]]
                        incoming =
                            [VisitorSupervisorIncomingWorkerAddedEvent ()
                            ,VisitorSupervisorIncomingWorkerStatusUpdateEvent (WorkerIdTagged () worker_status_update)
                            ,ReadVisitorSupervisorCurrentStatus
                            ]
                        correct_outgoing :: [[VisitorSupervisorOutgoingEvent () [Int]]]
                        correct_outgoing =
                            [[VisitorSupervisorOutgoingWorkloadEvent (WorkerIdTagged () entire_workload)]
                            ,newSolutionsEventsFromStatusUpdate status_update
                            ,[ResultVisitorSupervisorCurrentStatus status_update]
                            ]
                    in correct_outgoing @=? interpretSupervisorUsingModel incoming
                 -- }}}
                ,testProperty "recruitment, update (no new solutions), shutdown, recruitment" $ -- {{{
                  \(status_checkpoint :: VisitorCheckpoint)
                   (workload :: VisitorWorkload) → unsafePerformIO $
                    let incoming :: [VisitorSupervisorIncomingEvent () ()]
                        incoming =
                            [VisitorSupervisorIncomingWorkerAddedEvent ()
                            ,VisitorSupervisorIncomingWorkerStatusUpdateEvent (WorkerIdTagged () $ VisitorWorkerStatusUpdate
                                (VisitorStatusUpdate status_checkpoint mempty)
                                workload
                             )
                            ,VisitorSupervisorIncomingWorkerRemovedEvent ()
                            ,VisitorSupervisorIncomingWorkerAddedEvent ()
                            ]
                        correct_outgoing :: [[VisitorSupervisorOutgoingEvent () ()]]
                        correct_outgoing =
                            [[VisitorSupervisorOutgoingWorkloadEvent (WorkerIdTagged () entire_workload)]
                            ,[]
                            ,[]
                            ,[VisitorSupervisorOutgoingWorkloadEvent (WorkerIdTagged () workload)]
                            ]
                    in correct_outgoing @=? interpretSupervisorUsingModel incoming
                 -- }}}
                ,testProperty "recruitment, update, current checkpoint, shutdown, full checkpoint" $ -- {{{
                  \(worker_status_update@(VisitorWorkerStatusUpdate status_update workload) :: VisitorWorkerStatusUpdate [Int]) → unsafePerformIO $
                    let incoming :: [VisitorSupervisorIncomingEvent () [Int]]
                        incoming =
                            [VisitorSupervisorIncomingWorkerAddedEvent ()
                            ,VisitorSupervisorIncomingWorkerStatusUpdateEvent (WorkerIdTagged () worker_status_update)
                            ,ReadVisitorSupervisorCurrentStatus
                            ,VisitorSupervisorIncomingWorkerRemovedEvent ()
                            ,VisitorSupervisorIncomingRequestFullCheckpointEvent
                            ]
                        correct_outgoing :: [[VisitorSupervisorOutgoingEvent () [Int]]]
                        correct_outgoing =
                            [[VisitorSupervisorOutgoingWorkloadEvent (WorkerIdTagged () entire_workload)]
                            ,newSolutionsEventsFromStatusUpdate status_update
                            ,newSolutionsEventsFromStatusUpdate status_update ++ [ResultVisitorSupervisorCurrentStatus status_update]
                            ,[]
                            ,newSolutionsEventsFromStatusUpdate status_update ++ [ResultVisitorSupervisorCurrentStatus status_update]
                            ]
                    in correct_outgoing @=? interpretSupervisorUsingModel incoming
                 -- }}}
                ,testProperty "recruitment, full checkpoint, null, update" $ -- {{{
                  \(worker_status_update@(VisitorWorkerStatusUpdate status_update workload) :: VisitorWorkerStatusUpdate [Int]) → unsafePerformIO $
                    let incoming :: [VisitorSupervisorIncomingEvent () [Int]]
                        incoming =
                            [VisitorSupervisorIncomingWorkerAddedEvent ()
                            ,VisitorSupervisorIncomingRequestFullCheckpointEvent
                            ,VisitorSupervisorIncomingNullEvent
                            ,VisitorSupervisorIncomingWorkerStatusUpdateEvent (WorkerIdTagged () worker_status_update)
                            ]
                        correct_outgoing :: [[VisitorSupervisorOutgoingEvent () [Int]]]
                        correct_outgoing =
                            [[VisitorSupervisorOutgoingWorkloadEvent (WorkerIdTagged () entire_workload)]
                            ,[]
                            ,[]
                            ,VisitorSupervisorOutgoingCheckpointCompleteEvent status_update
                             :newSolutionsEventsFromStatusUpdate status_update
                            ]
                    in correct_outgoing @=? interpretSupervisorUsingModel incoming
                 -- }}}
                ]
             -- }}}
            ,testGroup "basic stealing events" -- {{{
                [testCase "two workers" $ -- {{{
                    let incoming :: [VisitorSupervisorIncomingEvent Bool [Int]]
                        incoming =
                            [VisitorSupervisorIncomingWorkerAddedEvent True
                            ,VisitorSupervisorIncomingWorkerAddedEvent False
                            ]
                        correct_outgoing :: [[VisitorSupervisorOutgoingEvent Bool [Int]]]
                        correct_outgoing =
                            [[VisitorSupervisorOutgoingWorkloadEvent (WorkerIdTagged True entire_workload)]
                            ,[VisitorSupervisorOutgoingBroadcastWorkerRequestEvent ([True],WorkloadStealReactiveRequest)]
                            ]
                    in correct_outgoing @=? interpretSupervisorUsingModel incoming
                 -- }}}
                ,testProperty "two workers, workload returned" $ -- {{{
                  \(stolen_workload@(VisitorWorkerStolenWorkload update workload) :: VisitorWorkerStolenWorkload [Int]) → unsafePerformIO $
                    let incoming :: [VisitorSupervisorIncomingEvent Bool [Int]]
                        incoming =
                            [VisitorSupervisorIncomingWorkerAddedEvent True
                            ,VisitorSupervisorIncomingWorkerAddedEvent False
                            ,VisitorSupervisorIncomingWorkerWorkloadStolenEvent (WorkerIdTagged True (Just stolen_workload))
                            ]
                        correct_outgoing :: [[VisitorSupervisorOutgoingEvent Bool [Int]]]
                        correct_outgoing =
                            [[VisitorSupervisorOutgoingWorkloadEvent (WorkerIdTagged True entire_workload)]
                            ,[VisitorSupervisorOutgoingBroadcastWorkerRequestEvent ([True],WorkloadStealReactiveRequest)]
                            ,[VisitorSupervisorOutgoingWorkloadEvent (WorkerIdTagged True workload)]
                            ]
                    in correct_outgoing @=? interpretSupervisorUsingModel incoming
                 -- }}}
                ,testCase "two workers, no workload returned" $ -- {{{
                    let incoming :: [VisitorSupervisorIncomingEvent Bool [Int]]
                        incoming =
                            [VisitorSupervisorIncomingWorkerAddedEvent True
                            ,VisitorSupervisorIncomingWorkerAddedEvent False
                            ,VisitorSupervisorIncomingWorkerWorkloadStolenEvent (WorkerIdTagged True Nothing)
                            ]
                        correct_outgoing :: [[VisitorSupervisorOutgoingEvent Bool [Int]]]
                        correct_outgoing =
                            [[VisitorSupervisorOutgoingWorkloadEvent (WorkerIdTagged True entire_workload)]
                            ,[VisitorSupervisorOutgoingBroadcastWorkerRequestEvent ([True],WorkloadStealReactiveRequest)]
                            ,[VisitorSupervisorOutgoingBroadcastWorkerRequestEvent ([True],WorkloadStealReactiveRequest)]
                            ]
                    in correct_outgoing @=? interpretSupervisorUsingModel incoming
                 -- }}}
                ,testCase "three workers" $ -- {{{
                    let incoming :: [VisitorSupervisorIncomingEvent Ordering [Int]]
                        incoming =
                            [VisitorSupervisorIncomingWorkerAddedEvent LT
                            ,VisitorSupervisorIncomingWorkerAddedEvent EQ
                            ,VisitorSupervisorIncomingWorkerAddedEvent GT
                            ]
                        correct_outgoing :: [[VisitorSupervisorOutgoingEvent Ordering [Int]]]
                        correct_outgoing =
                            [[VisitorSupervisorOutgoingWorkloadEvent (WorkerIdTagged LT entire_workload)]
                            ,[VisitorSupervisorOutgoingBroadcastWorkerRequestEvent ([LT],WorkloadStealReactiveRequest)]
                            ,[]
                            ]
                    in correct_outgoing @=? interpretSupervisorUsingModel incoming
                ]
             -- }}}
            ]
         -- }}}
         -- }}}
        ]
     -- }}}
    ,testGroup "Control.Monad.Trans.Visitor.Reactive.Worker" -- {{{
        [testGroup "absense of workload results in" -- {{{
            [testProperty "incoming workload starts the worker" $ \(visitor :: Visitor [Int]) → unsafePerformIO $ do -- {{{
                (event_handler,triggerEventWith) ← newAddHandler
                response_ivar ← IVar.new
                event_network ← compile $ do
                    event ← fromAddHandler event_handler
                    VisitorWorkerOutgoingEvents{..} ← createVisitorWorkerReactiveNetwork
                            (empty_worker_incoming_events { visitorWorkerIncomingWorkloadReceivedEvent = event })
                            visitor
                    reactimate (fmap (IVar.write response_ivar) visitorWorkerOutgoingFinishedEvent)
                    reactimate (fmap (const (IVar.write response_ivar (error "received visitorWorkerOutgoingMaybeStatusUpdatedEvent"))) visitorWorkerOutgoingMaybeStatusUpdatedEvent)
                    reactimate (fmap (const (IVar.write response_ivar (error "received visitorWorkerOutgoingMaybeWorkloadSubmittedEvent"))) visitorWorkerOutgoingMaybeWorkloadSubmittedEvent)
                    reactimate (fmap (const (IVar.write response_ivar (error "received visitorWorkerOutgoingFailureEvent"))) visitorWorkerOutgoingFailureEvent)
                actuate event_network
                triggerEventWith entire_workload
                response ← IVar.blocking $ IVar.read response_ivar
                pause event_network
                response @?= VisitorStatusUpdate Explored (runVisitor visitor)
                return True
             -- }}}
            ,testCase "null status update event" $ do -- {{{
                (event_handler,triggerEventWith) ← newAddHandler
                response_ivar ← IVar.new
                event_network ← compile $ do
                    event ← fromAddHandler event_handler
                    VisitorWorkerOutgoingEvents{..} ← createVisitorWorkerReactiveNetwork
                            (empty_worker_incoming_events { visitorWorkerIncomingRequestEvent = event })
                            (undefined :: Visitor [Int])
                    reactimate (fmap (IVar.write response_ivar) visitorWorkerOutgoingMaybeStatusUpdatedEvent)
                    reactimate (fmap (const (IVar.write response_ivar (error "received visitorWorkerOutgoingMaybeWorkloadSubmittedEvent"))) visitorWorkerOutgoingMaybeWorkloadSubmittedEvent)
                    reactimate (fmap (const (IVar.write response_ivar (error "received visitorWorkerOutgoingFinishedEvent"))) visitorWorkerOutgoingFinishedEvent)
                    reactimate (fmap (const (IVar.write response_ivar (error "received visitorWorkerOutgoingFailureEvent"))) visitorWorkerOutgoingFailureEvent)
                actuate event_network
                triggerEventWith StatusUpdateReactiveRequest
                response ← IVar.blocking $ IVar.read response_ivar
                pause event_network
                assertBool "is the status update Nothing?" $ isNothing response
             -- }}}
            ,testCase "null workload steal event" $ do -- {{{
                (event_handler,triggerEventWith) ← newAddHandler
                response_ivar ← IVar.new
                event_network ← compile $ do
                    event ← fromAddHandler event_handler
                    VisitorWorkerOutgoingEvents{..} ← createVisitorWorkerReactiveNetwork
                            (empty_worker_incoming_events { visitorWorkerIncomingRequestEvent = event })
                            (undefined :: Visitor [Int])
                    reactimate (fmap (IVar.write response_ivar) visitorWorkerOutgoingMaybeWorkloadSubmittedEvent)
                    reactimate (fmap (const (IVar.write response_ivar (error "received visitorWorkerOutgoingMaybeStatusUpdatedEvent"))) visitorWorkerOutgoingMaybeStatusUpdatedEvent)
                    reactimate (fmap (const (IVar.write response_ivar (error "received visitorWorkerOutgoingFinishedEvent"))) visitorWorkerOutgoingFinishedEvent)
                    reactimate (fmap (const (IVar.write response_ivar (error "received visitorWorkerOutgoingFailureEvent"))) visitorWorkerOutgoingFailureEvent)
                actuate event_network
                triggerEventWith WorkloadStealReactiveRequest
                response ← IVar.blocking $ IVar.read response_ivar
                pause event_network
                assertBool "is the workload nothing?" $ isNothing response
             -- }}}
            ]
         -- }}}
        ,testGroup "correct propagation of" -- {{{
            [testCase "exception in worker" $ do -- {{{
                (event_handler,triggerEventWith) ← newAddHandler
                response_ivar ← IVar.new
                let e = TestException 42
                event_network ← compile $ do
                    event ← fromAddHandler event_handler
                    VisitorWorkerOutgoingEvents{..} ← createVisitorWorkerReactiveNetwork
                            (empty_worker_incoming_events { visitorWorkerIncomingWorkloadReceivedEvent = event })
                            (throw e :: Visitor [Int])
                    reactimate (fmap (IVar.write response_ivar) visitorWorkerOutgoingFailureEvent)
                    reactimate (fmap (const (IVar.write response_ivar (error "received visitorWorkerOutgoingMaybeStatusUpdatedEvent"))) visitorWorkerOutgoingMaybeStatusUpdatedEvent)
                    reactimate (fmap (const (IVar.write response_ivar (error "received visitorWorkerOutgoingMaybeWorkloadSubmittedEvent"))) visitorWorkerOutgoingMaybeWorkloadSubmittedEvent)
                    reactimate (fmap (const (IVar.write response_ivar (error "received visitorWorkerOutgoingFinishedEvent"))) visitorWorkerOutgoingFinishedEvent)
                actuate event_network
                triggerEventWith entire_workload
                response ← IVar.blocking $ IVar.read response_ivar
                pause event_network
                fromException response @?= Just e
             -- }}}
            ,testCase "shutdown of worker" $ do -- {{{
                (event_handler,triggerEventWith) ← newAddHandler
                (shutdown_event_handler,triggerShutdownEvent) ← newAddHandler
                response_ref ← newIORef ()
                blocking_ivar ← IVar.new
                event_network ← compile $ do
                    event ← fromAddHandler event_handler
                    shutdown_event ← fromAddHandler shutdown_event_handler
                    VisitorWorkerOutgoingEvents{..} ← createVisitorIOWorkerReactiveNetwork
                            (empty_worker_incoming_events
                                { visitorWorkerIncomingShutdownEvent = shutdown_event
                                , visitorWorkerIncomingWorkloadReceivedEvent = event
                                }
                            )
                            (liftIO (IVar.blocking . IVar.read $ blocking_ivar) ⊕ return ())
                    reactimate (fmap (const (writeIORef response_ref (error "received visitorWorkerOutgoingMaybeStatusUpdatedEvent"))) visitorWorkerOutgoingMaybeStatusUpdatedEvent)
                    reactimate (fmap (const (writeIORef response_ref (error "received visitorWorkerOutgoingMaybeWorkloadSubmittedEvent"))) visitorWorkerOutgoingMaybeWorkloadSubmittedEvent)
                    reactimate (fmap (writeIORef response_ref . error . ("received visitorWorkerOutgoingFailureEvent: " ++) . show) visitorWorkerOutgoingFailureEvent)
                actuate event_network
                triggerEventWith entire_workload
                triggerShutdownEvent ()
                IVar.write blocking_ivar ()
                threadDelay 1000
                pause event_network
                readIORef response_ref >>= evaluate
             -- }}}
            ]
         -- }}}
        ,testGroup "correct response to" -- {{{
            [testProperty "status update request" $ flip fmap arbitrary $ \(visitor :: VisitorIO [Int]) → unsafePerformIO $ do -- {{{
                worker_started_qsem ← newQSem 0
                blocking_value_mvar ← newEmptyMVar
                let visitor_with_blocking_value =
                        ((liftIO (do
                            signalQSem worker_started_qsem
                            takeMVar blocking_value_mvar
                        ))
                        ⊕ return [24])
                        ⊕ visitor
                (status_update_1,final_status_update_1) ← do
                    termination_result_ivar ← IVar.new
                    VisitorWorkerEnvironment{..} ← forkVisitorIOWorkerThread
                        (IVar.write termination_result_ivar)
                        visitor_with_blocking_value
                        entire_workload
                    status_update_response_ref ← newIORef Nothing
                    waitQSem worker_started_qsem
                    writeIORef workerPendingRequests . Just . Seq.singleton . StatusUpdateRequested $ writeIORef status_update_response_ref . Just
                    putMVar blocking_value_mvar [42]
                    final_status_update ←
                        (IVar.blocking $ IVar.read termination_result_ivar)
                        >>=
                        \termination_result → case termination_result of
                            VisitorWorkerFinished final_status_update → return final_status_update
                            VisitorWorkerFailed exception → error ("worker threw exception: " ++ show exception)
                            VisitorWorkerAborted → error "worker aborted prematurely"
                    status_update_response ← readIORef status_update_response_ref
                    return (status_update_response,final_status_update)
                (status_update_2,final_status_update_2) ← do
                    (request_event_handler,triggerRequestEventWith) ← newAddHandler
                    (workload_event_handler,triggerWorkloadEventWith) ← newAddHandler
                    update_maybe_status_ivar ← IVar.new
                    workload_finished_ivar ← IVar.new
                    let e = TestException 42
                    event_network ← compile $ do
                        request_event ← fromAddHandler request_event_handler
                        workload_event ← fromAddHandler workload_event_handler
                        VisitorWorkerOutgoingEvents{..} ← createVisitorIOWorkerReactiveNetwork
                                (empty_worker_incoming_events
                                    { visitorWorkerIncomingRequestEvent = request_event
                                    , visitorWorkerIncomingWorkloadReceivedEvent = workload_event
                                    }
                                )
                                visitor_with_blocking_value
                        reactimate (fmap (IVar.write update_maybe_status_ivar) visitorWorkerOutgoingMaybeStatusUpdatedEvent)
                        reactimate (fmap (IVar.write workload_finished_ivar) visitorWorkerOutgoingFinishedEvent)
                        reactimate (fmap (const (IVar.write update_maybe_status_ivar (error "received visitorWorkerOutgoingMaybeWorkloadSubmittedEvent"))) visitorWorkerOutgoingMaybeWorkloadSubmittedEvent)
                        reactimate (fmap (const (IVar.write update_maybe_status_ivar (error "received visitorWorkerOutgoingFailureEvent"))) visitorWorkerOutgoingFailureEvent)
                    actuate event_network
                    triggerWorkloadEventWith entire_workload
                    waitQSem worker_started_qsem
                    triggerRequestEventWith StatusUpdateReactiveRequest
                    putMVar blocking_value_mvar [42]
                    update_maybe_status ← IVar.blocking $ IVar.read update_maybe_status_ivar
                    final_status_update ← IVar.blocking $ IVar.read workload_finished_ivar
                    pause event_network
                    return (update_maybe_status,final_status_update)
                status_update_1 @?= status_update_1
                final_status_update_1 @?= final_status_update_2
                return True
             -- }}}
            ,testProperty "workload steal request" $ flip fmap arbitrary $ \(visitor :: VisitorIO [Int]) → unsafePerformIO $ do -- {{{
                worker_started_qsem ← newQSem 0
                blocking_value_mvar ← newEmptyMVar
                let visitor_with_blocking_value =
                        ((liftIO (do
                            signalQSem worker_started_qsem
                            takeMVar blocking_value_mvar
                        ))
                        ⊕ return [24])
                        ⊕ visitor
                (steal_response_1,final_status_update_1) ← do
                    termination_result_ivar ← IVar.new
                    VisitorWorkerEnvironment{..} ← forkVisitorIOWorkerThread
                        (IVar.write termination_result_ivar)
                        visitor_with_blocking_value
                        entire_workload
                    workload_response_ref ← newIORef Nothing
                    waitQSem worker_started_qsem
                    writeIORef workerPendingRequests . Just . Seq.singleton . WorkloadStealRequested $ writeIORef workload_response_ref . Just
                    putMVar blocking_value_mvar [42]
                    final_status_update ←
                        (IVar.blocking $ IVar.read termination_result_ivar)
                        >>=
                        \termination_result → case termination_result of
                            VisitorWorkerFinished final_status_update → return final_status_update
                            VisitorWorkerFailed exception → error ("worker threw exception: " ++ show exception)
                            VisitorWorkerAborted → error "worker aborted prematurely"
                    workload_response ← readIORef workload_response_ref
                    return (workload_response,final_status_update)
                (steal_response_2,final_status_update_2) ← do
                    (request_event_handler,triggerRequestEventWith) ← newAddHandler
                    (workload_event_handler,triggerWorkloadEventWith) ← newAddHandler
                    steal_response_ivar ← IVar.new
                    workload_finished_ivar ← IVar.new
                    let e = TestException 42
                    event_network ← compile $ do
                        request_event ← fromAddHandler request_event_handler
                        workload_event ← fromAddHandler workload_event_handler
                        VisitorWorkerOutgoingEvents{..} ← createVisitorIOWorkerReactiveNetwork
                                (empty_worker_incoming_events
                                    { visitorWorkerIncomingRequestEvent = request_event
                                    , visitorWorkerIncomingWorkloadReceivedEvent = workload_event
                                    }
                                )
                                visitor_with_blocking_value
                        reactimate (fmap (IVar.write steal_response_ivar) visitorWorkerOutgoingMaybeWorkloadSubmittedEvent)
                        reactimate (fmap (IVar.write workload_finished_ivar) visitorWorkerOutgoingFinishedEvent)
                        reactimate (fmap (const (IVar.write steal_response_ivar (error "received visitorWorkerOutgoingMaybeStatusUpdatedEvent"))) visitorWorkerOutgoingMaybeStatusUpdatedEvent)
                        reactimate (fmap (const (IVar.write steal_response_ivar (error "received visitorWorkerOutgoingFailureEvent"))) visitorWorkerOutgoingFailureEvent)
                    actuate event_network
                    triggerWorkloadEventWith entire_workload
                    waitQSem worker_started_qsem
                    triggerRequestEventWith WorkloadStealReactiveRequest
                    putMVar blocking_value_mvar [42]
                    steal_response ← IVar.blocking $ IVar.read steal_response_ivar
                    final_status_update ← IVar.blocking $ IVar.read workload_finished_ivar
                    pause event_network
                    return (Just steal_response,final_status_update)
                steal_response_1 @?= steal_response_2
                final_status_update_1 @?= final_status_update_2
                return True
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
