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

import Control.Arrow ((&&&))
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class

import qualified Data.IntSet as IntSet
import Data.IORef
import Data.Maybe
import qualified Data.Sequence as Seq
import Data.Void (absurd)

import System.IO.Unsafe
import qualified System.Log.Logger as Logger
import System.Log.Logger (Priority(DEBUG),rootLoggerName,setLevel,updateGlobalLogger)
import System.Log.Logger.TH
import System.Random (randomIO)

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit hiding (Path,Test)
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Property (ioProperty)

import LogicGrowsOnTrees
import LogicGrowsOnTrees.Checkpoint
import LogicGrowsOnTrees.Parallel.ExplorationMode
import LogicGrowsOnTrees.Parallel.Purity
import LogicGrowsOnTrees.Path
import LogicGrowsOnTrees.Parallel.Common.Worker
import LogicGrowsOnTrees.Testing
import LogicGrowsOnTrees.Workload

main :: IO ()
main = do
    -- updateGlobalLogger rootLoggerName (setLevel DEBUG)
    defaultMain [tests]

tests :: Test
tests = testGroup "LogicGrowsOnTrees.Parallel.Common.Worker"
      [testGroup "forkWorkerThread"
          [testCase "abort" $ do
              termination_result_mvar ← newEmptyMVar
              semaphore ← newEmptyMVar
              WorkerEnvironment{..} ← forkWorkerThread AllMode io_purity
                  (putMVar termination_result_mvar)
                  (liftIO (takeMVar semaphore) `mplus` error "should never get here")
                  entire_workload
                  absurd
              sendAbortRequest workerPendingRequests
              putMVar semaphore ()
              termination_result ← takeMVar termination_result_mvar
              case termination_result of
                  WorkerFinished _ → assertFailure "worker faled to abort"
                  WorkerFailed exception → assertFailure ("worker threw exception: " ++ show exception)
                  WorkerAborted → return ()
              workerInitialPath @?= Seq.empty
              tryTakeMVar workerTerminationFlag >>= assertBool "is the termination flag set?" . isJust
          ,testGroup "obtains all solutions"
              [testProperty "with no initial path" $ \(tree :: Tree [Int]) → unsafePerformIO $ do
                  solutions_mvar ← newEmptyMVar
                  _ ← forkWorkerThread AllMode Pure
                          (putMVar solutions_mvar)
                          tree
                          entire_workload
                          absurd
                  Progress checkpoint solutions ←
                      (takeMVar solutions_mvar)
                      >>=
                      \termination_reason → case termination_reason of
                          WorkerFinished final_progress → return final_progress
                          other → error ("terminated unsuccessfully with reason " ++ show other)
                  checkpoint @?= Explored
                  solutions @?= exploreTree tree
                  return True
              ,testProperty "with an initial path" $ \(tree :: Tree [Int]) → randomPathForTree tree >>= \path → return . unsafePerformIO $ do
                  solutions_mvar ← newEmptyMVar
                  _ ← forkWorkerThread AllMode Pure
                          (putMVar solutions_mvar)
                          tree
                          (Workload path Unexplored)
                          absurd
                  Progress checkpoint solutions ←
                      (takeMVar solutions_mvar)
                      >>=
                      \termination_reason → case termination_reason of
                          WorkerFinished final_progress → return final_progress
                          other → error ("terminated unsuccessfully with reason " ++ show other)
                  checkpoint @?= checkpointFromInitialPath path Explored
                  solutions @?= (exploreTree . sendTreeDownPath path $ tree)
                  return True
              ]
          ,testGroup "progress updates correctly capture current and remaining progress" $
              let runAnalysis tree termination_flag termination_result_mvar progress_updates_ref = do
                      termination_result ← takeMVar termination_result_mvar
                      remaining_solutions ← case termination_result of
                          WorkerFinished (progressResult → solutions) → return solutions
                          WorkerFailed exception → error ("worker threw exception: " ++ show exception)
                          WorkerAborted → error "worker aborted prematurely"
                      tryTakeMVar termination_flag >>= assertBool "is the termination flag set?" . isJust
                      progress_updates ← reverse <$> readIORef progress_updates_ref
                      let correct_solutions = exploreTree tree
                          update_solutions = map (progressResult . progressUpdateProgress) progress_updates
                          all_solutions = remaining_solutions:update_solutions
                      forM_ (zip [0::Int ..] all_solutions) $ \(i,solutions_1) →
                          forM_ (zip [0::Int ..] all_solutions) $ \(j,solutions_2) →
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
              in
              [testProperty "continuous progress update requests" $ \(UniqueTree tree) → unsafePerformIO $ do
                  starting_flag ← newEmptyMVar
                  termination_result_mvar ← newEmptyMVar
                  WorkerEnvironment{..} ← forkWorkerThread AllMode io_purity
                      (putMVar termination_result_mvar)
                      ((liftIO . takeMVar $ starting_flag) >> endowTree tree)
                      entire_workload
                      absurd
                  progress_updates_ref ← newIORef []
                  let sendMyProgressUpdateRequest = sendProgressUpdateRequest workerPendingRequests submitProgressUpdate
                      submitProgressUpdate progress_update = do
                          atomicModifyIORef progress_updates_ref ((progress_update:) &&& const ())
                          sendMyProgressUpdateRequest
                  sendMyProgressUpdateRequest
                  putMVar starting_flag ()
                  runAnalysis tree workerTerminationFlag termination_result_mvar progress_updates_ref
              ,testProperty "progress update requests at random leaves" $ \(UniqueTree tree) → unsafePerformIO $ do
                  termination_result_mvar ← newEmptyMVar
                  progress_updates_ref ← newIORef []
                  rec WorkerEnvironment{..} ← forkWorkerThread AllMode io_purity
                          (putMVar termination_result_mvar)
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
                  runAnalysis tree workerTerminationFlag  termination_result_mvar progress_updates_ref
              ]
          ,testCase "terminates successfully with null tree" $ do
              termination_result_mvar ← newEmptyMVar
              WorkerEnvironment{..} ←
                  forkWorkerThread AllMode Pure
                      (putMVar termination_result_mvar)
                      (mzero :: Tree [Int])
                      entire_workload
                      absurd
              termination_result ← takeMVar termination_result_mvar
              case termination_result of
                  WorkerFinished (progressResult → solutions) → solutions @?= mempty
                  WorkerFailed exception → assertFailure ("worker threw exception: " ++ show exception)
                  WorkerAborted → assertFailure "worker prematurely aborted"
              workerInitialPath @?= Seq.empty
              tryTakeMVar workerTerminationFlag >>= assertBool "is the termination flag set?" . isJust
          ,testGroup "work stealing correctly preserves total workload" $
              let runManyStealsAnalysis tree termination_flag termination_result_mvar steals_ref = do
                      termination_result ← takeMVar termination_result_mvar
                      (Progress _ remaining_solutions) ← case termination_result of
                          WorkerFinished final_progress → return final_progress
                          WorkerFailed exception → error ("worker threw exception: " ++ show exception)
                          WorkerAborted → error "worker aborted prematurely"
                      tryTakeMVar termination_flag >>= assertBool "is the termination flag set?" . isJust
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
                      forM_ (zip [0::Int ..] all_solutions) $ \(i,solutions_1) →
                          forM_ (zip [0::Int ..] all_solutions) $ \(j,solutions_2) →
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
              in
                [testProperty "single steal" $ \(UniqueTree tree :: UniqueTree) → unsafePerformIO $ do -- {{{
                    reached_position_mvar ← newEmptyMVar
                    blocking_value_mvar ← newEmptyMVar
                    let tree_with_blocking_value =
                            mplus
                                (mplus
                                    (liftIO $ do
                                        _ ← tryPutMVar reached_position_mvar ()
                                        readMVar blocking_value_mvar
                                    )
                                    (return (IntSet.singleton 101010101))
                                )
                                (endowTree tree)
                    termination_result_mvar ← newEmptyMVar
                    WorkerEnvironment{..} ← forkWorkerThread AllMode io_purity
                        (putMVar termination_result_mvar)
                        tree_with_blocking_value
                        entire_workload
                        absurd
                    maybe_workload_ref ← newIORef Nothing
                    takeMVar reached_position_mvar
                    sendWorkloadStealRequest workerPendingRequests $ writeIORef maybe_workload_ref
                    putMVar blocking_value_mvar (IntSet.singleton 202020202)
                    Progress _ remaining_solutions ←
                        takeMVar termination_result_mvar
                        >>=
                        \termination_result → case termination_result of
                            WorkerFinished final_progress → return final_progress
                            WorkerFailed exception → error ("worker threw exception: " ++ show exception)
                            WorkerAborted → error "worker aborted prematurely"
                    isEmptyMVar workerTerminationFlag >>= assertBool "is the termination flag set?" . not
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
              ,testProperty "continuous stealing" $ \(UniqueTree tree) → unsafePerformIO $ do
                  starting_flag ← newEmptyMVar
                  termination_result_mvar ← newEmptyMVar
                  WorkerEnvironment{..} ← forkWorkerThread AllMode io_purity
                      (putMVar termination_result_mvar)
                      ((liftIO . takeMVar $ starting_flag) >> endowTree tree)
                      entire_workload
                      absurd
                  steals_ref ← newIORef []
                  let submitMyWorkloadStealRequest = sendWorkloadStealRequest workerPendingRequests submitStolenWorkload
                      submitStolenWorkload Nothing = submitMyWorkloadStealRequest
                      submitStolenWorkload (Just steal) = do
                          atomicModifyIORef steals_ref ((steal:) &&& const ())
                          submitMyWorkloadStealRequest
                  submitMyWorkloadStealRequest
                  putMVar starting_flag ()
                  runManyStealsAnalysis tree workerTerminationFlag termination_result_mvar steals_ref
              ,testProperty "stealing at random leaves" $ \(UniqueTree tree) → unsafePerformIO $ do
                  termination_result_mvar ← newEmptyMVar
                  steals_ref ← newIORef []
                  rec WorkerEnvironment{..} ← forkWorkerThread AllMode io_purity
                          (putMVar termination_result_mvar)
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
                  runManyStealsAnalysis tree workerTerminationFlag termination_result_mvar steals_ref
              ]
          ]
      ,testProperty "exploreTreeUntilFirst" $ \(tree :: Tree String) → ioProperty $ do
          termination_reason ← exploreTreeGeneric FirstMode Pure tree
          case termination_reason of
              WorkerFinished maybe_final_progress → return $ (progressResult <$> maybe_final_progress) == exploreTreeUntilFirst tree
              _ → fail $ "returned " ++ show termination_reason ++ " instead of WorkerFinished"
      ]
