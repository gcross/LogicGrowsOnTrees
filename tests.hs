-- @+leo-ver=5-thin
-- @+node:gcross.20101114125204.1259: * @file tests.hs
-- @@language haskell

-- @+<< Language extensions >>
-- @+node:gcross.20101114125204.1281: ** << Language extensions >>
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- @-<< Language extensions >>

-- @+<< Import needed modules >>
-- @+node:gcross.20101114125204.1260: ** << Import needed modules >>
import Control.Concurrent.QSem
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Operational (ProgramViewT(..),view)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Writer

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Functor.Identity
import Data.IORef
import qualified Data.IVar as IVar
import Data.List (mapAccumL)
import Data.Maybe
import Data.Sequence (Seq,(<|),(|>),(><))
import qualified Data.Sequence as Seq
import Data.Serialize (Serialize,decode,encode)

import Debug.Trace (trace)

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
import Control.Monad.Trans.Visitor.Worker
-- @-<< Import needed modules >>

-- @+others
-- @+node:gcross.20111028153100.1302: ** Arbitrary
-- @+node:gcross.20111028170027.1297: *3* Branch
instance Arbitrary Branch where arbitrary = elements [LeftBranch,RightBranch]
-- @+node:gcross.20111028170027.1307: *3* Visitor
instance (Serialize α, Arbitrary α) ⇒ Arbitrary (Visitor α) where
    arbitrary = sized arb
      where
        arb :: Int → Gen (Visitor α)
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
        null, result, cached :: Gen (Visitor α)
        null = return mzero
        result = fmap return arbitrary
        cached = fmap cache arbitrary

        bindToArbitrary :: Int → Gen (Visitor α) → Gen (Visitor α)
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
-- @+node:gcross.20111028170027.1298: ** Functions
-- @+node:gcross.20111028170027.1299: *3* echo
echo :: Show α ⇒ α → α
echo x = trace (show x) x
-- @+node:gcross.20111028170027.1301: *3* echoWithLabel
echoWithLabel :: Show α ⇒ String → α → α
echoWithLabel label x = trace (label ++ " " ++ show x) x
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
        -- @+node:gcross.20111028153100.1286: *4* runVisitorWithCheckpointing
        ,testGroup "runVisitorWithCheckpointing"
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
                            (runVisitorWithCheckpointing v)
                in all (== final_results) results_using_progressive_checkpoints
            -- @+node:gcross.20111028170027.1315: *5* example instances
            ,testGroup "example instances"
                -- @+others
                -- @+node:gcross.20111028153100.1291: *6* mplus
                [testGroup "mplus"
                    -- @+others
                    -- @+node:gcross.20111028153100.1293: *7* mzero + mzero
                    [testCase "mzero + mzero" $
                        runVisitorWithCheckpointing (mzero `mplus` mzero :: Visitor Int)
                        @?=
                        [(Nothing,Unexplored)
                        ,(Nothing,ChoiceCheckpoint Explored Unexplored)
                        ]
                    -- @+node:gcross.20111028153100.1297: *7* mzero + return
                    ,testCase "mzero + return" $
                        runVisitorWithCheckpointing (mzero `mplus` return 1 :: Visitor Int)
                        @?=
                        [(Nothing,Unexplored)
                        ,(Nothing,ChoiceCheckpoint Explored Unexplored)
                        ,(Just (VisitorSolution (labelFromBranching [RightBranch]) 1),Explored)
                        ]
                    -- @+node:gcross.20111028153100.1298: *7* return + mzero
                    -- @+at
                    --  ,testCase "return + mzero" $
                    --      runVisitorWithCheckpointing (return 1 `mplus` mzero :: Visitor Int)
                    --      @?=
                    --      [(Nothing,Unexplored)
                    --      ,(Nothing,ChoiceCheckpoint Explored Unexplored)
                    --      ,(Just (VisitorSolution (labelFromBranching [LeftBranch]) 1),Explored)
                    --      ]
                    -- @-others
                    ]
                -- @+node:gcross.20111028153100.1287: *6* mzero
                ,testCase "mzero" $ runVisitorWithCheckpointing (mzero :: Visitor Int) @?= []
                -- @+node:gcross.20111028153100.1289: *6* return
                ,testCase "return" $ runVisitorWithCheckpointing (return 0 :: Visitor Int) @?= [(Just (VisitorSolution rootLabel 0),Explored)]
                -- @-others
                ]
            -- @+node:gcross.20111028170027.1316: *5* same results as runVisitor
            ,testProperty "same results as runVisitor" $ \(v :: Visitor Int) →
                runVisitor v == fmap visitorSolutionResult (mapMaybe fst (runVisitorWithCheckpointing v))
            -- @-others
            ]
        -- @-others
        ]
    -- @+node:gcross.20111029192420.1341: *3* Control.Monad.Trans.Visitor.Label
    ,testGroup "Control.Monad.Trans.Visitor.Label"
        -- @+others
        -- @+node:gcross.20111029192420.1342: *4* runVisitorWithLabels
        [testGroup "runVisitorWithLabels"
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
    -- @+node:gcross.20111028181213.1319: *3* Control.Monad.Trans.Visitor.Worker
    ,testGroup "Control.Monad.Trans.Visitor.Worker"
        -- @+others
        -- @+node:gcross.20111028181213.1333: *4* forkVisitorIOWorkerThread
        [testGroup "forkVisitorIOWorkerThread"
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
            -- @-others
            ]
        -- @+node:gcross.20111028181213.1320: *4* forkVisitorWorkerThread
        ,testGroup "forkVisitorWorkerThread"
            -- @+others
            -- @+node:gcross.20111028181213.1321: *5* obtains all solutions
            [testGroup "obtains all solutions"
                -- @+others
                -- @+node:gcross.20111028181213.1327: *6* with an initial path
                [testProperty "with an initial path" $ \(visitor :: Visitor Int) → randomPathForVisitor visitor >>= \path → return . unsafePerformIO $ do
                    solutions_ivar ← IVar.new
                    worker_environment ←
                        forkVisitorWorkerThread
                            (IVar.write solutions_ivar)
                            visitor
                            (VisitorWorkload path Unexplored)
                    worker_solutions ← IVar.blocking $ IVar.read solutions_ivar
                    return $
                        case worker_solutions of
                            VisitorWorkerFinished solutions
                                | map visitorSolutionResult solutions ==
                                  (
                                    map visitorSolutionResult
                                    .
                                    runVisitorWithLabels
                                    .
                                    walkVisitorDownPath path
                                    $
                                    visitor
                                  )
                              → True
                            _ → False
                -- @+node:gcross.20111028181213.1322: *6* with no initial path
                ,testProperty "with no initial path" $ \(visitor :: Visitor Int) → unsafePerformIO $ do
                    solutions_ivar ← IVar.new
                    worker_environment ←
                        forkVisitorWorkerThread
                            (IVar.write solutions_ivar)
                            visitor
                            entire_workload
                    worker_solutions ← IVar.blocking $ IVar.read solutions_ivar
                    return $
                        case worker_solutions of
                            VisitorWorkerFinished solutions
                                | solutions == runVisitorWithLabels visitor
                              → True
                            _ → False
                -- @-others
                ]
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
                    VisitorWorkerFinished solutions → solutions @?= []
                    VisitorWorkerFailed exception → assertFailure ("worker threw exception: " ++ show exception)
                    VisitorWorkerAborted → assertFailure "worker prematurely aborted"
                workerInitialPath @?= Seq.empty
                readIORef workerPendingRequests >>= assertBool "has the request queue been nulled?" . isNothing
            -- @-others
            ]
        -- @-others
        ]
    -- @-others
    -- @-<< Tests >>
    ]
-- @-leo
