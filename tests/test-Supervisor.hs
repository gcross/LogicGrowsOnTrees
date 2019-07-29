{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Monad
import Control.Monad.IO.Class

import Data.IORef
import Data.List (sort)
import Data.Monoid
import Data.Sequence ((|>))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.UUID (UUID)

import System.IO.Unsafe
import qualified System.Log.Logger as Logger
import System.Log.Logger (Priority(DEBUG),rootLoggerName,setLevel,updateGlobalLogger)
import System.Log.Logger.TH

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit hiding (Path,Test)
import Test.QuickCheck.Arbitrary hiding ((><))
import Test.QuickCheck.Gen hiding (shuffle)
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Modifiers

import LogicGrowsOnTrees.Checkpoint
import LogicGrowsOnTrees.Parallel.ExplorationMode
import LogicGrowsOnTrees.Path
import LogicGrowsOnTrees.Parallel.Common.Supervisor
import LogicGrowsOnTrees.Parallel.Common.Worker
import LogicGrowsOnTrees.Testing
import LogicGrowsOnTrees.Workload

deriveLoggers "Logger" [DEBUG]

main :: IO ()
main = do
    -- updateGlobalLogger rootLoggerName (setLevel DEBUG)
    defaultMain [tests]

tests :: Test
tests = testGroup "LogicGrowsOnTrees.Parallel.Common.Supervisor"
    [testCase "immediately abort" $ do
        SupervisorOutcome{..} ← runSupervisor AllMode bad_test_supervisor_actions (TestProgram abortSupervisor)
        supervisorTerminationReason @?= SupervisorAborted (Progress Unexplored ())
        supervisorRemainingWorkers @?= ([] :: [Int])
    ,testCase "failure" $ do
        SupervisorOutcome{..} ← runTestSupervisor AllMode bad_test_supervisor_actions (receiveWorkerFailure () "FAIL" :: ∀ α. SupervisorMonad (AllMode ()) () IO α)
        supervisorTerminationReason @?= SupervisorFailure mempty () "FAIL"
        supervisorRemainingWorkers @?= []
    ,testGroup "adding and removing workers"
        [testGroup "without workload buffer"
            [testCase "add one worker then abort" $ do
                (maybe_workload_ref,actions) ← addAcceptOneWorkloadAction bad_test_supervisor_actions
                SupervisorOutcome{..} ← runTestSupervisor AllMode actions $ do
                    enableSupervisorDebugMode
                    setWorkloadBufferSize 0
                    addWorker ()
                    abortSupervisor
                supervisorTerminationReason @?= SupervisorAborted (Progress Unexplored ())
                supervisorRemainingWorkers @?= [()]
                readIORef maybe_workload_ref >>= (@?= Just ((),entire_workload))
            ,testCase "add then remove one worker then abort" $ do
                (maybe_workload_ref,actions) ← addAcceptOneWorkloadAction bad_test_supervisor_actions
                SupervisorOutcome{..} ← runTestSupervisor AllMode actions $ do
                    enableSupervisorDebugMode
                    setWorkloadBufferSize 0
                    addWorker ()
                    removeWorker ()
                    abortSupervisor
                supervisorTerminationReason @?= SupervisorAborted (Progress Unexplored ())
                supervisorRemainingWorkers @?= []
                readIORef maybe_workload_ref >>= (@?= Just ((),entire_workload)) 
            ,testCase "add then remove then add one worker then abort" $ do
                (maybe_workload_ref,actions) ← addAcceptMultipleWorkloadsAction bad_test_supervisor_actions
                SupervisorOutcome{..} ← runTestSupervisor AllMode actions $ do
                    enableSupervisorDebugMode
                    setWorkloadBufferSize 0
                    addWorker 1
                    removeWorker 1
                    addWorker 2
                    abortSupervisor
                supervisorTerminationReason @?= SupervisorAborted (Progress Unexplored ())
                supervisorRemainingWorkers @?= [2::Int]
                readIORef maybe_workload_ref >>= (@?= [(1,entire_workload),(2,entire_workload)]) 
            ,testCase "add two workers then remove first worker then abort" $ do
                (maybe_workload_ref,actions1) ← addAcceptMultipleWorkloadsAction bad_test_supervisor_actions
                (broadcast_ids_list_ref,actions2) ← addAppendWorkloadStealBroadcastIdsAction actions1
                SupervisorOutcome{..} ← runTestSupervisor AllMode actions2 $ do
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
            ,testProperty "add then remove many workers then abort" $ do
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
                return . unsafePerformIO $ do
                    (maybe_workload_ref,actions_1) ← addAcceptOneWorkloadAction bad_test_supervisor_actions
                    (broadcast_ids_list_ref,actions_2) ← addAppendWorkloadStealBroadcastIdsAction actions_1
                    SupervisorOutcome{..} ← runTestSupervisor AllMode actions_2 $ do
                        enableSupervisorDebugMode
                        setWorkloadBufferSize 0
                        mapM_ addWorker worker_ids_to_add
                        mapM_ removeWorker worker_ids_to_remove
                        abortSupervisor
                    supervisorTerminationReason @?= SupervisorAborted (Progress Unexplored ())
                    sort supervisorRemainingWorkers @?= worker_ids_left
                    readIORef maybe_workload_ref >>= (@?= Just (head worker_ids_to_add,entire_workload))
                    readIORef broadcast_ids_list_ref >>= (@?= if (null . tail) worker_ids_to_add then [] else [[head worker_ids_to_add]])
                    return True
            ]
        ,testGroup "with workload buffer"
            [testCase "add one worker then abort" $ do
                (maybe_workload_ref,actions1) ← addAcceptOneWorkloadAction bad_test_supervisor_actions
                (broadcasts_ref,actions2) ← addAppendWorkloadStealBroadcastIdsAction actions1
                SupervisorOutcome{..} ← runTestSupervisor AllMode actions2 $ do
                    enableSupervisorDebugMode
                    addWorker ()
                    abortSupervisor
                supervisorTerminationReason @?= SupervisorAborted (Progress Unexplored ())
                supervisorRemainingWorkers @?= [()]
                readIORef maybe_workload_ref >>= (@?= Just ((),entire_workload))
                readIORef broadcasts_ref >>= (@?= [[()]])
            ,testCase "add then remove one worker then abort" $ do
                (maybe_workload_ref,actions1) ← addAcceptOneWorkloadAction bad_test_supervisor_actions
                (broadcasts_ref,actions2) ← addAppendWorkloadStealBroadcastIdsAction actions1
                SupervisorOutcome{..} ← runTestSupervisor AllMode actions2 $ do
                    enableSupervisorDebugMode
                    addWorker ()
                    removeWorker ()
                    abortSupervisor
                supervisorTerminationReason @?= SupervisorAborted (Progress Unexplored ())
                supervisorRemainingWorkers @?= []
                readIORef maybe_workload_ref >>= (@?= Just ((),entire_workload)) 
                readIORef broadcasts_ref >>= (@?= [[()]])
            ,testCase "add then remove then add one worker then abort" $ do
                (maybe_workload_ref,actions1) ← addAcceptMultipleWorkloadsAction bad_test_supervisor_actions
                (broadcasts_ref,actions2) ← addAppendWorkloadStealBroadcastIdsAction actions1
                SupervisorOutcome{..} ← runTestSupervisor AllMode actions2 $ do
                    enableSupervisorDebugMode
                    addWorker 1
                    removeWorker 1
                    addWorker 2
                    abortSupervisor
                supervisorTerminationReason @?= SupervisorAborted (Progress Unexplored ())
                supervisorRemainingWorkers @?= [2::Int]
                readIORef maybe_workload_ref >>= (@?= [(1,entire_workload),(2,entire_workload)]) 
                readIORef broadcasts_ref >>= (@?= [[1],[2]])
            ]
        ]
    ,testGroup "progress updates"
        [testCase "request progress update when no workers present" $ do
            (maybe_progress_ref,actions) ← addReceiveCurrentProgressAction bad_test_supervisor_actions
            SupervisorOutcome{..} ← runTestSupervisor AllMode actions $ do
                enableSupervisorDebugMode
                setWorkloadBufferSize 0
                performGlobalProgressUpdate
                abortSupervisor
            supervisorTerminationReason @?= SupervisorAborted (Progress Unexplored ())
            supervisorRemainingWorkers @?= ([] :: [()])
            readIORef maybe_progress_ref >>= (@?= Just (Progress Unexplored ()))
        ,testProperty "request progress update when all active workers present leave" $ do
            number_of_active_workers ← choose (1,10 :: Int)
            number_of_inactive_workers ← choose (0,10)
            let active_workers = [0..number_of_active_workers-1]
                inactive_workers = [101..101+number_of_inactive_workers-1]
            return . unsafePerformIO $ do
                (maybe_progress_ref,actions1) ← addReceiveCurrentProgressAction bad_test_supervisor_actions
                (broadcast_ids_list_ref,actions2) ← addAppendProgressBroadcastIdsAction actions1
                (workload_steal_ids_ref,actions3) ← addSetWorkloadStealBroadcastIdsAction actions2
                let actions4 = ignoreAcceptWorkloadAction $ actions3
                let progress = Progress Unexplored (Sum (0::Int))
                SupervisorOutcome{..} ← runTestSupervisor AllMode actions4 $ do
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
                return True
        ,testCase "request and receive Just progress update when one worker present" $ do
            (maybe_progress_ref,actions1) ← addReceiveCurrentProgressAction bad_test_supervisor_actions
            (broadcast_ids_list_ref,actions2) ← addAppendProgressBroadcastIdsAction actions1
            let actions3 = ignoreAcceptWorkloadAction actions2
            let progress = Progress (ChoicePoint Unexplored Unexplored) (Sum (1::Int))
            SupervisorOutcome{..} ← runTestSupervisor AllMode actions3 $ do
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
        ,testCase "request and receive progress update when active and inactive workers present" $ do
            (maybe_progress_ref,actions1) ← addReceiveCurrentProgressAction bad_test_supervisor_actions
            (broadcast_ids_list_ref,actions2) ← addAppendProgressBroadcastIdsAction actions1
            let actions3 = ignoreAcceptWorkloadAction . ignoreWorkloadStealAction $ actions2
            let progress = Progress (ChoicePoint Unexplored Unexplored) (Sum (1::Int))
            SupervisorOutcome{..} ← runTestSupervisor AllMode actions3 $ do
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
        ]
    ,testGroup "workload steals"
        [testCase "failure to steal from a worker leads to second attempt" $ do 
            (broadcast_ids_list_ref,actions1) ← addAppendWorkloadStealBroadcastIdsAction bad_test_supervisor_actions
            let actions2 = ignoreAcceptWorkloadAction actions1
            SupervisorOutcome{..} ← runTestSupervisor AllMode actions2 $ do
                addWorker (1::Int)
                addWorker 2
                receiveStolenWorkload 1 Nothing
                abortSupervisor
            supervisorTerminationReason @?= SupervisorAborted (Progress Unexplored ())
            supervisorRemainingWorkers @?= [1,2]
            readIORef broadcast_ids_list_ref >>= (@?= [[1],[1]])
        ]
    ,testCase "starting from previous checkpoint" $ do
        (maybe_workload_ref,actions1) ← addAcceptOneWorkloadAction bad_test_supervisor_actions
        (broadcast_ids_list_ref,actions2) ← addAppendWorkloadStealBroadcastIdsAction actions1
        let checkpoint = ChoicePoint Unexplored Unexplored
            progress = Progress checkpoint (Sum (1::Int))
        SupervisorOutcome{..} ← runTestSupervisorStartingFrom AllMode progress actions2 $ do
            addWorker ()
            abortSupervisor
        supervisorTerminationReason @?= SupervisorAborted progress
        supervisorRemainingWorkers @?= [()]
        readIORef maybe_workload_ref >>= (@?= Just ((),(Workload Seq.empty checkpoint)))
        readIORef broadcast_ids_list_ref >>= (@?= [[()]])
    ,testGroup "FirstMode" $
        [testGroup "single worker"
            [testCase "finishes with Explored" $ do
                SupervisorOutcome{..} ← runTestSupervisor FirstMode ignore_supervisor_actions $ do
                    enableSupervisorDebugMode
                    setWorkloadBufferSize 0
                    addWorker ()
                    receiveWorkerFinished () (Progress Explored Nothing)
                    error "Supervisor did not terminate"
                supervisorTerminationReason @?= SupervisorCompleted (Nothing :: Maybe (Progress ()))
            ,testCase "finishes with result" $ do
                SupervisorOutcome{..} ← runTestSupervisor FirstMode ignore_supervisor_actions $ do
                    enableSupervisorDebugMode
                    setWorkloadBufferSize 0
                    addWorker ()
                    receiveWorkerFinished () (Progress Explored (Just ()))
                    error "Supervisor did not terminate"
                supervisorTerminationReason @?= SupervisorCompleted (Just (Progress Explored ()))
            ]
        ,testGroup "two workers"
            [testCase "both finish with Explored" $ do
                SupervisorOutcome{..} ← runTestSupervisor FirstMode ignore_supervisor_actions $ do
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
            ,testCase "both finish with result" $ do
                SupervisorOutcome{..} ← runTestSupervisor FirstMode ignore_supervisor_actions $ do
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
            ]
        ]
    ,testCase "worker count listener" $ do
        count_1_ref ← newIORef (-1)
        count_2_ref ← newIORef (-1)
        _ ← runTestSupervisor AllMode ignore_supervisor_actions $ (do
            addWorkerCountListener $ writeIORef count_1_ref
            liftIO $ readIORef count_1_ref >>= (@?= 0)
            addWorker (0::Int)
            liftIO $ readIORef count_1_ref >>= (@?= 1)
            addWorkerCountListener $ writeIORef count_2_ref
            liftIO $ readIORef count_2_ref >>= (@?= 1)
            addWorker 1
            liftIO $ readIORef count_1_ref >>= (@?= 2)
            liftIO $ readIORef count_2_ref >>= (@?= 2)
            removeWorker 0
            liftIO $ readIORef count_1_ref >>= (@?= 1)
            liftIO $ readIORef count_2_ref >>= (@?= 1)
            addWorker 2
            liftIO $ readIORef count_1_ref >>= (@?= 2)
            liftIO $ readIORef count_2_ref >>= (@?= 2)
            receiveStolenWorkload 1 . Just $
                StolenWorkload
                    (ProgressUpdate
                        (Progress (ChoicePoint Unexplored Unexplored) ())
                        (Workload (Seq.fromList [ChoiceStep LeftBranch]) Unexplored)
                    )
                    (Workload (Seq.fromList [ChoiceStep RightBranch]) Unexplored)
            receiveWorkerFinishedAndRemoved 1 (Progress (ChoicePoint Explored Unexplored) ())
            liftIO $ readIORef count_1_ref >>= (@?= 1)
            liftIO $ readIORef count_2_ref >>= (@?= 1)
            abortSupervisor
          :: ∀ α. SupervisorMonad (AllMode ()) Int IO α)
        readIORef count_1_ref >>= (@?= 0)
        readIORef count_2_ref >>= (@?= 0)
        return ()
    ]
