-- @+leo-ver=5-thin
-- @+node:gcross.20101114125204.1259: * @file tests.hs
-- @@language haskell

-- @+<< Language extensions >>
-- @+node:gcross.20101114125204.1281: ** << Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- @-<< Language extensions >>

-- @+<< Import needed modules >>
-- @+node:gcross.20101114125204.1260: ** << Import needed modules >>
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
import Data.Function
import Data.Functor.Identity
import Data.IORef
import qualified Data.IVar as IVar
import Data.List (mapAccumL,sort)
import Data.List.Unicode
import Data.Maybe
import Data.Monoid
import Data.Monoid.Unicode
import Data.Sequence (Seq,(<|),(|>),(><))
import qualified Data.Sequence as Seq
import Data.Serialize (Serialize,decode,encode)

import Debug.Trace (trace)

import Reactive.Banana

import System.IO.Unsafe

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck.Arbitrary hiding ((><))
import Test.QuickCheck.Gen

import Control.Monad.Trans.Visitor
import Control.Monad.Trans.Visitor.Checkpoint
import Control.Monad.Trans.Visitor.Label
import Control.Monad.Trans.Visitor.Path
import Control.Monad.Trans.Visitor.Reactive.Worker
import Control.Monad.Trans.Visitor.Workload
import Control.Monad.Trans.Visitor.Worker
-- @-<< Import needed modules >>

-- @+others
-- @+node:gcross.20111028153100.1302: ** Arbitrary
-- @+node:gcross.20111028170027.1297: *3* Branch
instance Arbitrary Branch where arbitrary = elements [LeftBranch,RightBranch]
-- @+node:gcross.20111028170027.1307: *3* Visitor
instance (Serialize α, Arbitrary α, Monad m) ⇒ Arbitrary (VisitorT m α) where
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
-- @+node:gcross.20111028153100.1303: *3* VisitorCheckpoint
instance Arbitrary VisitorCheckpoint where
    arbitrary = sized arb
      where
        arb 0 = elements [Explored,Unexplored]
        arb n = frequency
                    [(1,return Explored)
                    ,(1,return Unexplored)
                    ,(1,liftM2 CacheCheckpoint (fmap encode (arbitrary :: Gen Int)) (arb (n-1)))
                    ,(2,liftM2 ChoiceCheckpoint (arb (n `div` 2)) (arb (n `div` 2)))
                    ]
-- @+node:gcross.20111029212714.1360: *3* VisitorLabel
instance Arbitrary VisitorLabel where arbitrary = fmap labelFromBranching (arbitrary :: Gen [Branch])
-- @+node:gcross.20111116214909.1383: *3* VisitorPath
instance Arbitrary VisitorPath where
    arbitrary = fmap Seq.fromList . listOf . oneof $ [fmap (CacheStep . encode) (arbitrary :: Gen Int),fmap ChoiceStep arbitrary]
-- @+node:gcross.20111117140347.1401: ** Exceptions
-- @+node:gcross.20111117140347.1402: *3* TestException
data TestException = TestException Int deriving (Eq,Show,Typeable)

instance Exception TestException
-- @+node:gcross.20111028170027.1298: ** Functions
-- @+node:gcross.20111028170027.1299: *3* echo
echo :: Show α ⇒ α → α
echo x = trace (show x) x
-- @+node:gcross.20111028170027.1301: *3* echoWithLabel
echoWithLabel :: Show α ⇒ String → α → α
echoWithLabel label x = trace (label ++ " " ++ show x) x
-- @+node:gcross.20111029212714.1372: *3* randomCheckpointForVisitor
randomCheckpointForVisitor :: Visitor α → Gen VisitorCheckpoint
randomCheckpointForVisitor (VisitorT visitor) = go1 visitor
  where
    go1 visitor = frequency
        [(1,return Explored)
        ,(1,return Unexplored)
        ,(3,go2 visitor)
        ]
    go2 (view → Cache c :>>= k) =
        fmap (CacheCheckpoint (encode . runIdentity $ c)) (go1 (k (runIdentity c)))
    go2 (view → Choice (VisitorT x) (VisitorT y) :>>= k) =
        liftM2 ChoiceCheckpoint (go1 (x >>= k)) (go1 (y >>= k))
    go2 _ = elements [Explored,Unexplored]
-- @+node:gcross.20111028181213.1315: *3* randomPathForVisitor
randomPathForVisitor :: Visitor α → Gen VisitorPath
randomPathForVisitor (VisitorT visitor) = go visitor
  where
    go (view → Cache c :>>= k) = oneof
        [return Seq.empty
        ,fmap (CacheStep (encode . runIdentity $ c) <|) (go (k (runIdentity c)))
        ]
    go (view → Choice (VisitorT x) (VisitorT y) :>>= k) = oneof
        [return Seq.empty
        ,fmap (ChoiceStep LeftBranch <|) (go (x >>= k))
        ,fmap (ChoiceStep RightBranch <|) (go (y >>= k))
        ]
    go _ = return Seq.empty
-- @+node:gcross.20111028181213.1318: *3* randomVisitorWithoutCache
randomVisitorWithoutCache :: Arbitrary α ⇒ Gen (Visitor α)
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
-- @-others

main = defaultMain
    -- @+<< Tests >>
    -- @+node:gcross.20101114125204.1267: ** << Tests >>
    -- @+others
    -- @+node:gcross.20110923164140.1187: *3* Control.Monad.Trans.Visitor
    [testGroup "Control.Monad.Trans.Visitor"
        -- @+others
        -- @+node:gcross.20111028170027.1312: *4* Eq instance
        [testGroup "Eq instance"
            -- @+others
            -- @+node:gcross.20111028170027.1313: *5* self
            [testProperty "self" $ \(v :: Visitor Int) → v == v
            -- @-others
            ]
        -- @+node:gcross.20110722110408.1173: *4* runVisitor
        ,testGroup "runVisitor"
            -- @+others
            -- @+node:gcross.20110722110408.1177: *5* return
            [testCase "return" $ runVisitor (return ()) @?= [()]
            -- @+node:gcross.20110722110408.1174: *5* mzero
            ,testCase "mzero" $ runVisitor (mzero :: Visitor ()) @?= []
            -- @+node:gcross.20110722110408.1178: *5* mplus
            ,testCase "mplus" $ runVisitor (return 1 `mplus` return 2) @?= [1,2]
            -- @+node:gcross.20110722110408.1179: *5* cache
            ,testCase "cache" $ runVisitor (cache 42) @?= [42::Int]
            -- @-others
            ]
        -- @+node:gcross.20110923164140.1202: *4* runVisitorT
        ,testGroup "runVisitorT"
            -- @+others
            -- @+node:gcross.20110923164140.1203: *5* Writer
            [testCase "Writer" $
                (runWriter . runVisitorT $ do
                    cache [1 :: Int] >>= lift . tell
                    (lift (tell [2]) `mplus` lift (tell [3]))
                    return 42
                ) @?= ((),[1,2,3])
            -- @-others
            ]
        -- @+node:gcross.20110923164140.1211: *4* runVisitorTAndGatherResults
        ,testGroup "runVisitorTAndGatherResults"
            -- @+others
            -- @+node:gcross.20110923164140.1212: *5* Writer
            [testCase "Writer" $
                (runWriter . runVisitorTAndGatherResults $ do
                    cache [1 :: Int] >>= lift . tell
                    (lift (tell [2]) `mplus` lift (tell [3]))
                    return 42
                ) @?= ([42,42],[1,2,3])
            -- @-others
            ]
        -- @-others
        ]
    -- @+node:gcross.20111028153100.1285: *3* Control.Monad.Trans.Visitor.Checkpoint
    ,testGroup "Control.Monad.Trans.Visitor.Checkpoint"
        -- @+others
        -- @+node:gcross.20111028153100.1300: *4* contextFromCheckpoint
        [testGroup "contextFromCheckpoint"
            -- @+others
            -- @+node:gcross.20111028170027.1296: *5* branch
            [testProperty "branch" $ \(checkpoint :: VisitorCheckpoint) (active_branch :: Branch) →
                checkpointFromContext (Seq.singleton (BranchContextStep active_branch)) checkpoint
                ==
                (mergeCheckpointRoot $ case active_branch of
                    LeftBranch → ChoiceCheckpoint checkpoint Explored
                    RightBranch → ChoiceCheckpoint Explored checkpoint)
            -- @+node:gcross.20111028170027.1303: *5* cache
            ,testProperty "cache" $ \(checkpoint :: VisitorCheckpoint) (i :: Int) →
                checkpointFromContext (Seq.singleton (CacheContextStep (encode i))) checkpoint
                ==
                (mergeCheckpointRoot $ CacheCheckpoint (encode i) checkpoint)
            -- @+node:gcross.20111028170027.1305: *5* choice
            ,testProperty "choice" $ \(inner_checkpoint :: VisitorCheckpoint) (other_visitor :: Visitor Int) (other_checkpoint :: VisitorCheckpoint) →
                (checkpointFromContext (Seq.singleton (LeftChoiceContextStep other_checkpoint other_visitor)) inner_checkpoint)
                ==
                (mergeCheckpointRoot $ ChoiceCheckpoint inner_checkpoint other_checkpoint)
            -- @+node:gcross.20111028170027.1295: *5* empty
            ,testProperty "empty" $ \(checkpoint :: VisitorCheckpoint) →
                checkpointFromContext Seq.empty checkpoint == checkpoint
            -- @-others
            ]
        -- @+node:gcross.20111117140347.1419: *4* Monoid instance
        ,testGroup "Monoid instance"
            -- @+others
            -- @+node:gcross.20111117140347.1421: *5* product results in intersection of solutions
            [testProperty "product results in intersection of solutions" $ \(visitor :: Visitor Int) → do
                checkpoint1 ← randomCheckpointForVisitor visitor
                checkpoint2 ← randomCheckpointForVisitor visitor
                let checkpoint3 = checkpoint1 ⊕ checkpoint2
                    solutions1 = runVisitorThroughCheckpointAndGatherResults checkpoint1 visitor
                    solutions2 = runVisitorThroughCheckpointAndGatherResults checkpoint2 visitor
                    solutions3 = runVisitorThroughCheckpointAndGatherResults checkpoint3 visitor
                return $ solutions3 == solutions1 ∩ solutions2
            -- @+node:gcross.20111117140347.1431: *5* throws the correct exceptions
            ,testCase "throws the correct exceptions" $
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
            -- @+node:gcross.20111117140347.1420: *5* unit element laws
            ,testProperty "unit element laws" $ \(checkpoint :: VisitorCheckpoint) →
                mempty ⊕ checkpoint == checkpoint && checkpoint ⊕ mempty == checkpoint
            -- @-others
            ]
        -- @+node:gcross.20111029212714.1374: *4* runVisitorThroughCheckpoint
        ,testGroup "runVisitorThroughCheckpoint"
            -- @+others
            -- @+node:gcross.20111029212714.1376: *5* matches walk down path
            [testProperty "matches walk down path" $ \(visitor :: Visitor Int) → randomPathForVisitor visitor >>= \path → return $
                runVisitorWithStartingLabel (labelFromPath path) (walkVisitorDownPath path visitor)
                ==
                mapMaybe fst (runVisitorThroughCheckpoint (checkpointFromUnexploredPath path) visitor)
            -- @-others
            ]
        -- @+node:gcross.20111028153100.1286: *4* runVisitorWithCheckpoints
        ,testGroup "runVisitorWithCheckpoints"
            -- @+others
            -- @+node:gcross.20111028181213.1308: *5* checkpoints accurately capture remaining search space
            [testProperty "checkpoints accurately capture remaining search space" $ \(v :: Visitor Int) →
                let (final_results,results_using_progressive_checkpoints) =
                        mapAccumL
                            (\solutions_so_far (maybe_new_solution,checkpoint) →
                                let new_solutions :: Seq (VisitorSolution Int)
                                    new_solutions = maybe solutions_so_far (solutions_so_far |>) maybe_new_solution
                                in (new_solutions,new_solutions >< Seq.fromList (mapMaybe fst (runVisitorThroughCheckpoint checkpoint v)))
                            )
                            Seq.empty
                            (runVisitorWithCheckpoints v)
                in all (== final_results) results_using_progressive_checkpoints
            -- @+node:gcross.20111028170027.1315: *5* example instances
            ,testGroup "example instances"
                -- @+others
                -- @+node:gcross.20111028153100.1291: *6* mplus
                [testGroup "mplus"
                    -- @+others
                    -- @+node:gcross.20111028153100.1293: *7* mzero + mzero
                    [testCase "mzero + mzero" $
                        runVisitorWithCheckpoints (mzero `mplus` mzero :: Visitor Int)
                        @?=
                        [(Nothing,Unexplored)
                        ,(Nothing,ChoiceCheckpoint Explored Unexplored)
                        ]
                    -- @+node:gcross.20111028153100.1297: *7* mzero + return
                    ,testCase "mzero + return" $
                        runVisitorWithCheckpoints (mzero `mplus` return 1 :: Visitor Int)
                        @?=
                        [(Nothing,Unexplored)
                        ,(Nothing,ChoiceCheckpoint Explored Unexplored)
                        ,(Just (VisitorSolution (labelFromBranching [RightBranch]) 1),Explored)
                        ]
                    -- @+node:gcross.20111028153100.1298: *7* return + mzero
                    -- @+at
                    --  ,testCase "return + mzero" $
                    --      runVisitorWithCheckpoints (return 1 `mplus` mzero :: Visitor Int)
                    --      @?=
                    --      [(Nothing,Unexplored)
                    --      ,(Nothing,ChoiceCheckpoint Explored Unexplored)
                    --      ,(Just (VisitorSolution (labelFromBranching [LeftBranch]) 1),Explored)
                    --      ]
                    -- @-others
                    ]
                -- @+node:gcross.20111028153100.1287: *6* mzero
                ,testCase "mzero" $ runVisitorWithCheckpoints (mzero :: Visitor Int) @?= []
                -- @+node:gcross.20111028153100.1289: *6* return
                ,testCase "return" $ runVisitorWithCheckpoints (return 0 :: Visitor Int) @?= [(Just (VisitorSolution rootLabel 0),Explored)]
                -- @-others
                ]
            -- @+node:gcross.20111028170027.1316: *5* same results as runVisitor
            ,testProperty "same results as runVisitor" $ \(v :: Visitor Int) →
                runVisitor v == fmap visitorSolutionResult (mapMaybe fst (runVisitorWithCheckpoints v))
            -- @+node:gcross.20111029192420.1345: *5* same results as runVisitorWithLabels
            ,testProperty "same results as runVisitorWithLabels" $ \(v :: Visitor Int) →
                runVisitorWithLabels v == mapMaybe fst (runVisitorWithCheckpoints v)
            -- @-others
            ]
        -- @-others
        ]
    -- @+node:gcross.20111029192420.1341: *3* Control.Monad.Trans.Visitor.Label
    ,testGroup "Control.Monad.Trans.Visitor.Label"
        -- @+others
        -- @+node:gcross.20111029212714.1356: *4* branchingFromLabel . labelFromBranching = id
        [testProperty "branchingFromLabel . labelFromBranching = id" $
            liftA2 (==)
                (branchingFromLabel . labelFromBranching)
                id
        -- @+node:gcross.20111029212714.1362: *4* labelFromBranching . branchingFromLabel = id
        ,testProperty "labelFromBranching . branchingFromLabel = id" $
            liftA2 (==)
                (labelFromBranching . branchingFromLabel)
                id
        -- @+node:gcross.20111029192420.1362: *4* Monoid instance
        ,testGroup "Monoid instance"
            -- @+others
            -- @+node:gcross.20111029192420.1363: *5* equivalent to concatenation of branchings
            [testProperty "equivalent to concatenation of branchings" $ \(parent_branching :: [Branch]) (child_branching :: [Branch]) →
                labelFromBranching parent_branching ⊕ labelFromBranching child_branching
                ==
                labelFromBranching (parent_branching ⊕ child_branching)
            -- @+node:gcross.20111029212714.1357: *5* obeys monoid laws
            ,testProperty "obeys monoid laws" $
                liftA2 (&&)
                    (liftA2 (==) id (⊕ (mempty :: VisitorLabel)))
                    (liftA2 (==) id ((mempty :: VisitorLabel) ⊕))
            -- @-others
            ]
        -- @+node:gcross.20111116214909.1379: *4* Ord instance of VisitorLabel equivalent to Ord of branching
        ,testProperty "Ord instance of VisitorLabel equivalent to Ord of branching" $ \a b →
            (compare `on` branchingFromLabel) a b == compare a b
        -- @+node:gcross.20111029192420.1342: *4* runVisitorWithLabels
        ,testGroup "runVisitorWithLabels"
            -- @+others
            -- @+node:gcross.20111029192420.1344: *5* same result as walking down path
            [testProperty "same result as walking down path" $ \(visitor :: Visitor Int) →
                runVisitor visitor == fmap visitorSolutionResult (runVisitorWithLabels visitor)
            -- @-others
            ]
        -- @+node:gcross.20111028181213.1313: *4* walkVisitorDownLabel
        ,testGroup "walkVisitorDownLabel"
            -- @+others
            -- @+node:gcross.20111028181213.1316: *5* same result as walking down path
            [testProperty "same result as walking down path" $ do
                visitor :: Visitor Int ← randomVisitorWithoutCache
                path ← randomPathForVisitor visitor
                let label = labelFromPath path
                return $
                    walkVisitorDownPath path visitor
                    ==
                    walkVisitorDownLabel label visitor
            -- @-others
            ]
        -- @-others
        ]
    -- @+node:gcross.20110923164140.1188: *3* Control.Monad.Trans.Visitor.Path
    ,testGroup "Control.Monad.Trans.Visitor.Path"
        -- @+others
        -- @+node:gcross.20110923164140.1189: *4* walkVisitorDownPath
        [testGroup "walkVisitorDownPath"
            -- @+others
            -- @+node:gcross.20110923164140.1191: *5* null path
            [testCase "null path" $ (runVisitor . walkVisitorDownPath Seq.empty) (return 42) @?= [42]
            -- @+node:gcross.20110923164140.1200: *5* cache
            ,testCase "cache" $ do (runVisitor . walkVisitorDownPath (Seq.singleton (CacheStep (encode (42 :: Int))))) (cache (undefined :: Int)) @?= [42]
            -- @+node:gcross.20110923164140.1199: *5* choice
            ,testCase "choice" $ do
                (runVisitor . walkVisitorDownPath (Seq.singleton (ChoiceStep LeftBranch))) (return 42 `mplus` undefined) @?= [42]
                (runVisitor . walkVisitorDownPath (Seq.singleton (ChoiceStep RightBranch))) (undefined `mplus` return 42) @?= [42]
            -- @+node:gcross.20110923164140.1192: *5* errors
            ,testGroup "errors"
                -- @+others
                -- @+node:gcross.20111028181213.1310: *6* PastVisitorIsInconsistentWithPresentVisitor
                [testGroup "PastVisitorIsInconsistentWithPresentVisitor"
                    -- @+others
                    -- @+node:gcross.20110923164140.1198: *7* cache step with choice
                    [testCase "cache step with choice" $
                        try (
                            evaluate
                            .
                            runVisitor
                            $
                            walkVisitorDownPath (Seq.singleton (CacheStep undefined :: VisitorStep)) (undefined `mplus` undefined :: Visitor Int)
                        ) >>= (@?= Left PastVisitorIsInconsistentWithPresentVisitor)
                    -- @+node:gcross.20110923164140.1196: *7* choice step with cache
                    ,testCase "choice step with cache" $
                        try (
                            evaluate
                            .
                            runVisitor
                            $
                            walkVisitorDownPath (Seq.singleton (ChoiceStep undefined :: VisitorStep)) (cache undefined :: Visitor Int)
                        ) >>= (@?= Left PastVisitorIsInconsistentWithPresentVisitor)
                    -- @-others
                    ]
                -- @+node:gcross.20111028181213.1311: *6* VisitorTerminatedBeforeEndOfWalk
                ,testGroup "VisitorTerminatedBeforeEndOfWalk"
                    -- @+others
                    -- @+node:gcross.20110923164140.1195: *7* mzero
                    [testCase "mzero" $
                        try (
                            evaluate
                            .
                            runVisitor
                            $
                            walkVisitorDownPath (Seq.singleton (undefined :: VisitorStep)) (mzero :: Visitor Int)
                        ) >>= (@?= Left VisitorTerminatedBeforeEndOfWalk)
                    -- @+node:gcross.20110923164140.1193: *7* return
                    ,testCase "return" $
                        try (
                            evaluate
                            .
                            runVisitor
                            $
                            walkVisitorDownPath (Seq.singleton (undefined :: VisitorStep)) (return (undefined :: Int))
                        ) >>= (@?= Left VisitorTerminatedBeforeEndOfWalk)
                    -- @-others
                    ]
                -- @-others
                ]
            -- @-others
            ]
        -- @+node:gcross.20110923164140.1220: *4* walkVisitorTDownPath
        ,testGroup "walkVisitorT"
            -- @+others
            -- @+node:gcross.20110923164140.1223: *5* cache step
            [testCase "cache step" $ do
                let (transformed_visitor,log) =
                        runWriter . walkVisitorTDownPath (Seq.singleton (CacheStep . encode $ (24 :: Int))) $ do
                            runAndCache (tell [1] >> return (42 :: Int) :: Writer [Int] Int)
                log @?= []
                (runWriter . runVisitorTAndGatherResults $ transformed_visitor) @?= ([24],[])
            -- @+node:gcross.20110923164140.1221: *5* choice step
            ,testCase "choice step" $ do
                let (transformed_visitor,log) =
                        runWriter . walkVisitorTDownPath (Seq.singleton (ChoiceStep RightBranch)) $ do
                            lift (tell [1])
                            (lift (tell [2]) `mplus` lift (tell [3]))
                            lift (tell [4])
                            return 42
                log @?= [1]
                (runWriter . runVisitorTAndGatherResults $ transformed_visitor) @?= ([42],[3,4])
            -- @-others
            ]
        -- @-others
        ]
    -- @+node:gcross.20111116214909.1384: *3* Control.Monad.Trans.Visitor.Reactive.Worker
    ,testGroup "Control.Monad.Trans.Visitor.Reactive.Worker"
        -- @+others
        -- @+node:gcross.20111116214909.1388: *4* absense of workload results in
        [testGroup "absense of workload results in"
            -- @+others
            -- @+node:gcross.20111117140347.1384: *5* incoming workload starts the worker
            [testProperty "incoming workload starts the worker" $ \(visitor :: Visitor Int) → unsafePerformIO $ do
                (event_handler,triggerEventWith) ← newAddHandler
                response_ivar ← IVar.new
                event_network ← compile $ do
                    event ← fromAddHandler event_handler
                    VisitorWorkerOutgoingEvents{..} ← createVisitorWorkerReactiveNetwork
                            (mempty { visitorWorkerIncomingWorkloadReceivedEvent = event })
                            visitor
                    reactimate (fmap (IVar.write response_ivar) visitorWorkerOutgoingFinishedEvent)
                    reactimate (fmap (const (IVar.write response_ivar (error "received visitorWorkerOutgoingMaybeStatusUpdatedEvent"))) visitorWorkerOutgoingMaybeStatusUpdatedEvent)
                    reactimate (fmap (const (IVar.write response_ivar (error "received visitorWorkerOutgoingMaybeWorkloadSubmittedEvent"))) visitorWorkerOutgoingMaybeWorkloadSubmittedEvent)
                    reactimate (fmap (const (IVar.write response_ivar (error "received visitorWorkerOutgoingFailureEvent"))) visitorWorkerOutgoingFailureEvent)
                actuate event_network
                triggerEventWith entire_workload
                response ← IVar.blocking $ IVar.read response_ivar
                pause event_network
                response @?= VisitorWorkerFinalUpdate (runVisitorWithLabels visitor) Explored
                return True
            -- @+node:gcross.20111116214909.1385: *5* null status update event
            ,testCase "null status update event" $ do
                (event_handler,triggerEventWith) ← newAddHandler
                response_ivar ← IVar.new
                event_network ← compile $ do
                    event ← fromAddHandler event_handler
                    VisitorWorkerOutgoingEvents{..} ← createVisitorWorkerReactiveNetwork
                            (mempty { visitorWorkerIncomingRequestEvent = event })
                            undefined
                    reactimate (fmap (IVar.write response_ivar) visitorWorkerOutgoingMaybeStatusUpdatedEvent)
                    reactimate (fmap (const (IVar.write response_ivar (error "received visitorWorkerOutgoingMaybeWorkloadSubmittedEvent"))) visitorWorkerOutgoingMaybeWorkloadSubmittedEvent)
                    reactimate (fmap (const (IVar.write response_ivar (error "received visitorWorkerOutgoingFinishedEvent"))) visitorWorkerOutgoingFinishedEvent)
                    reactimate (fmap (const (IVar.write response_ivar (error "received visitorWorkerOutgoingFailureEvent"))) visitorWorkerOutgoingFailureEvent)
                actuate event_network
                triggerEventWith StatusUpdateReactiveRequest
                response ← IVar.blocking $ IVar.read response_ivar
                pause event_network
                assertBool "is the status update Nothing?" $ isNothing response
            -- @+node:gcross.20111116214909.1387: *5* null workload steal event
            ,testCase "null workload steal event" $ do
                (event_handler,triggerEventWith) ← newAddHandler
                response_ivar ← IVar.new
                event_network ← compile $ do
                    event ← fromAddHandler event_handler
                    VisitorWorkerOutgoingEvents{..} ← createVisitorWorkerReactiveNetwork
                            (mempty { visitorWorkerIncomingRequestEvent = event })
                            undefined
                    reactimate (fmap (IVar.write response_ivar) visitorWorkerOutgoingMaybeWorkloadSubmittedEvent)
                    reactimate (fmap (const (IVar.write response_ivar (error "received visitorWorkerOutgoingMaybeStatusUpdatedEvent"))) visitorWorkerOutgoingMaybeStatusUpdatedEvent)
                    reactimate (fmap (const (IVar.write response_ivar (error "received visitorWorkerOutgoingFinishedEvent"))) visitorWorkerOutgoingFinishedEvent)
                    reactimate (fmap (const (IVar.write response_ivar (error "received visitorWorkerOutgoingFailureEvent"))) visitorWorkerOutgoingFailureEvent)
                actuate event_network
                triggerEventWith WorkloadStealReactiveRequest
                response ← IVar.blocking $ IVar.read response_ivar
                pause event_network
                assertBool "is the workload nothing?" $ isNothing response
            -- @-others
            ]
        -- @+node:gcross.20111117140347.1439: *4* correct propagation of
        ,testGroup "correct propagation of"
            -- @+others
            -- @+node:gcross.20111117140347.1405: *5* exception in worker
            [testCase "exception in worker" $ do
                (event_handler,triggerEventWith) ← newAddHandler
                response_ivar ← IVar.new
                let e = TestException 42
                event_network ← compile $ do
                    event ← fromAddHandler event_handler
                    VisitorWorkerOutgoingEvents{..} ← createVisitorWorkerReactiveNetwork
                            (mempty { visitorWorkerIncomingWorkloadReceivedEvent = event })
                            (throw e)
                    reactimate (fmap (IVar.write response_ivar) visitorWorkerOutgoingFailureEvent)
                    reactimate (fmap (const (IVar.write response_ivar (error "received visitorWorkerOutgoingMaybeStatusUpdatedEvent"))) visitorWorkerOutgoingMaybeStatusUpdatedEvent)
                    reactimate (fmap (const (IVar.write response_ivar (error "received visitorWorkerOutgoingMaybeWorkloadSubmittedEvent"))) visitorWorkerOutgoingMaybeWorkloadSubmittedEvent)
                    reactimate (fmap (const (IVar.write response_ivar (error "received visitorWorkerOutgoingFinishedEvent"))) visitorWorkerOutgoingFinishedEvent)
                actuate event_network
                triggerEventWith entire_workload
                response ← IVar.blocking $ IVar.read response_ivar
                pause event_network
                fromException response @?= Just e
            -- @+node:gcross.20111117140347.1407: *5* shutdown of worker
            ,testCase "shutdown of worker" $ do
                (event_handler,triggerEventWith) ← newAddHandler
                (shutdown_event_handler,triggerShutdownEvent) ← newAddHandler
                response_ref ← newIORef ()
                blocking_ivar ← IVar.new
                event_network ← compile $ do
                    event ← fromAddHandler event_handler
                    shutdown_event ← fromAddHandler shutdown_event_handler
                    VisitorWorkerOutgoingEvents{..} ← createVisitorIOWorkerReactiveNetwork
                            (mempty { visitorWorkerIncomingShutdownEvent = shutdown_event
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
            -- @-others
            ]
        -- @+node:gcross.20111117140347.1440: *4* correct response to
        ,testGroup "correct response to"
            -- @+others
            -- @+node:gcross.20111117140347.1446: *5* status update request
            [testProperty "status update request" $ flip fmap arbitrary $ \(visitor :: VisitorIO Int) → unsafePerformIO $ do
                worker_started_qsem ← newQSem 0
                blocking_value_mvar ← newEmptyMVar
                let visitor_with_blocking_value =
                        ((liftIO (do
                            signalQSem worker_started_qsem
                            takeMVar blocking_value_mvar
                        ))
                        ⊕ return 24)
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
                    putMVar blocking_value_mvar 42
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
                                (mempty { visitorWorkerIncomingRequestEvent = request_event
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
                    putMVar blocking_value_mvar 42
                    update_maybe_status ← IVar.blocking $ IVar.read update_maybe_status_ivar
                    final_status_update ← IVar.blocking $ IVar.read workload_finished_ivar
                    pause event_network
                    return (update_maybe_status,final_status_update)
                status_update_1 @?= status_update_1
                final_status_update_1 @?= final_status_update_2
                return True
            -- @+node:gcross.20111117140347.1444: *5* workload steal request
            ,testProperty "workload steal request" $ flip fmap arbitrary $ \(visitor :: VisitorIO Int) → unsafePerformIO $ do
                worker_started_qsem ← newQSem 0
                blocking_value_mvar ← newEmptyMVar
                let visitor_with_blocking_value =
                        ((liftIO (do
                            signalQSem worker_started_qsem
                            takeMVar blocking_value_mvar
                        ))
                        ⊕ return 24)
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
                    putMVar blocking_value_mvar 42
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
                                (mempty { visitorWorkerIncomingRequestEvent = request_event
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
                    putMVar blocking_value_mvar 42
                    steal_response ← IVar.blocking $ IVar.read steal_response_ivar
                    final_status_update ← IVar.blocking $ IVar.read workload_finished_ivar
                    pause event_network
                    return (Just steal_response,final_status_update)
                steal_response_1 @?= steal_response_2
                final_status_update_1 @?= final_status_update_2
                return True
            -- @-others
            ]
        -- @-others
        ]
    -- @+node:gcross.20111028181213.1319: *3* Control.Monad.Trans.Visitor.Worker
    ,testGroup "Control.Monad.Trans.Visitor.Worker"
        -- @+others
        -- @+node:gcross.20111028181213.1320: *4* forkVisitor(X)WorkerThread
        [testGroup "forkVisitor(X)WorkerThread"
            -- @+others
            -- @+node:gcross.20111028181213.1334: *5* abort
            [testCase "abort" $ do
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
            -- @+node:gcross.20111028181213.1321: *5* obtains all solutions
            ,testGroup "obtains all solutions"
                -- @+others
                -- @+node:gcross.20111028181213.1327: *6* with an initial path
                [testProperty "with an initial path" $ \(visitor :: Visitor Int) → randomPathForVisitor visitor >>= \path → return . unsafePerformIO $ do
                    solutions_ivar ← IVar.new
                    worker_environment ←
                        forkVisitorWorkerThread
                            (IVar.write solutions_ivar)
                            visitor
                            (VisitorWorkload path Unexplored)
                    VisitorWorkerFinalUpdate solutions checkpoint ←
                        (IVar.blocking $ IVar.read solutions_ivar)
                        >>=
                        \termination_reason → case termination_reason of
                            VisitorWorkerFinished final_status_update → return final_status_update
                            other → error ("terminated unsuccessfully with reason " ++ show other)
                    checkpoint @?= checkpointFromInitialPath path Explored
                    ((@?=) `on` (map visitorSolutionResult))
                        solutions
                        (runVisitorWithLabels . walkVisitorDownPath path $ visitor)
                    return True
                -- @+node:gcross.20111028181213.1322: *6* with no initial path
                ,testProperty "with no initial path" $ \(visitor :: Visitor Int) → unsafePerformIO $ do
                    solutions_ivar ← IVar.new
                    worker_environment ←
                        forkVisitorWorkerThread
                            (IVar.write solutions_ivar)
                            visitor
                            entire_workload
                    VisitorWorkerFinalUpdate solutions checkpoint ←
                        (IVar.blocking $ IVar.read solutions_ivar)
                        >>=
                        \termination_reason → case termination_reason of
                            VisitorWorkerFinished final_status_update → return final_status_update
                            other → error ("terminated unsuccessfully with reason " ++ show other)
                    checkpoint @?= Explored
                    solutions @?= runVisitorWithLabels visitor
                    return True

                -- @-others
                ]
            -- @+node:gcross.20111028181213.1339: *5* status updates produce valid checkpoints
            ,testProperty "status updates produce valid checkpoints" $ \(visitor :: Visitor Int) → unsafePerformIO $ do
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
                    VisitorWorkerFinished (visitorWorkerFinalNewSolutions → solutions) → return solutions
                    VisitorWorkerFailed exception → error ("worker threw exception: " ++ show exception)
                    VisitorWorkerAborted → error "worker aborted prematurely"
                readIORef workerPendingRequests >>= assertBool "has the request queue been nulled?" . isNothing
                checkpoints ← fmap DList.toList (readIORef checkpoints_ref)
                let correct_solutions = runVisitorWithLabels visitor
                correct_solutions @=? ((++ remaining_solutions) . concat . fmap visitorWorkerNewSolutions $ checkpoints)
                forM_ checkpoints $
                    \(VisitorWorkerStatusUpdate _ (VisitorWorkload initial_path _) _) →
                        assertBool
                            "checkpoint has null initial path"
                            (Seq.null initial_path)
                let (almost_final_solutions,solutions_using_progressive_checkpoints) =
                        mapAccumL
                            (\solutions_so_far (VisitorWorkerStatusUpdate new_solutions workload _) →
                                let new_solutions_so_far :: [VisitorSolution Int]
                                    new_solutions_so_far = solutions_so_far ++ new_solutions
                                in  (new_solutions_so_far
                                    ,new_solutions_so_far ++ mapMaybe fst (runVisitorThroughWorkload workload visitor)
                                    )
                            )
                            []
                            checkpoints
                almost_final_solutions ++ remaining_solutions @?= correct_solutions
                forM_ solutions_using_progressive_checkpoints $ (@?= correct_solutions)
                return True
            -- @+node:gcross.20111028181213.1325: *5* terminates successfully with null visitor
            ,testCase "terminates successfully with null visitor" $ do
                termination_result_ivar ← IVar.new
                VisitorWorkerEnvironment{..} ←
                    forkVisitorWorkerThread
                        (IVar.write termination_result_ivar)
                        (mzero :: Visitor Int)
                        entire_workload
                termination_result ← IVar.blocking $ IVar.read termination_result_ivar
                case termination_result of
                    VisitorWorkerFinished (visitorWorkerFinalNewSolutions → solutions) → solutions @?= []
                    VisitorWorkerFailed exception → assertFailure ("worker threw exception: " ++ show exception)
                    VisitorWorkerAborted → assertFailure "worker prematurely aborted"
                workerInitialPath @?= Seq.empty
                readIORef workerPendingRequests >>= assertBool "has the request queue been nulled?" . isNothing
            -- @+node:gcross.20111116214909.1369: *5* work stealing correctly preserves total workload
            ,testGroup "work stealing correctly preserves total workload"
                -- @+others
                -- @+node:gcross.20111116214909.1371: *6* single steal
                [testProperty "single steal" $ flip fmap arbitrary $ \(visitor :: VisitorIO Int) → unsafePerformIO $ do
                    worker_started_qsem ← newQSem 0
                    blocking_value_ivar ← IVar.new
                    let visitor_with_blocking_value =
                            ((liftIO (do
                                signalQSem worker_started_qsem
                                IVar.blocking . IVar.read $ blocking_value_ivar
                            ))
                            ⊕ return 24)
                            ⊕ visitor
                    termination_result_ivar ← IVar.new
                    VisitorWorkerEnvironment{..} ← forkVisitorIOWorkerThread
                        (IVar.write termination_result_ivar)
                        visitor_with_blocking_value
                        entire_workload
                    maybe_maybe_workload_ref ← newIORef Nothing
                    waitQSem worker_started_qsem
                    writeIORef workerPendingRequests . Just . Seq.singleton . WorkloadStealRequested $ writeIORef maybe_maybe_workload_ref . Just
                    IVar.write blocking_value_ivar 42
                    VisitorWorkerFinalUpdate remaining_solutions checkpoint ←
                        (IVar.blocking $ IVar.read termination_result_ivar)
                        >>=
                        \termination_result → case termination_result of
                            VisitorWorkerFinished final_status_update → return final_status_update
                            VisitorWorkerFailed exception → error ("worker threw exception: " ++ show exception)
                            VisitorWorkerAborted → error "worker aborted prematurely"
                    readIORef workerPendingRequests >>= assertBool "has the request queue been nulled?" . isNothing
                    (VisitorWorkerStatusUpdate prestolen_solutions remaining_workload checkpoint,workload) ←
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
                    let solutions = sort (prestolen_solutions ++ remaining_solutions ++ stolen_solutions)
                    correct_solutions ← runVisitorTWithLabelsAndGatherResults visitor_with_blocking_value
                    solutions @?= correct_solutions
                    return True
                -- @+node:gcross.20111116214909.1375: *6* continuous stealing
                ,testProperty "continuous stealing" $ \(visitor :: Visitor Int) → unsafePerformIO $ do
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
                        VisitorWorkerFinished (visitorWorkerFinalNewSolutions → solutions) → return solutions
                        VisitorWorkerFailed exception → error ("worker threw exception: " ++ show exception)
                        VisitorWorkerAborted → error "worker aborted prematurely"
                    workloads ← fmap (map snd . DList.toList) (readIORef workloads_ref)
                    let stolen_solutions = concatMap (flip runVisitorThroughWorkloadAndGatherResults visitor) $ workloads
                        solutions = sort (remaining_solutions ++ stolen_solutions)
                        correct_solutions = runVisitorWithLabels visitor
                    solutions @?= correct_solutions
                    return True
                -- @-others
                ]
            -- @-others
            ]
        -- @-others
        ]
    -- @-others
    -- @-<< Tests >>
    ]
-- @-leo
