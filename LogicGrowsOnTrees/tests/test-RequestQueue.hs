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

import Control.Concurrent
import Control.Exception (AsyncException(ThreadKilled))
import Control.Monad
import Control.Monad.Catch (Exception(fromException),try)
import Control.Monad.IO.Class

import Data.Time.Clock (NominalDiffTime,diffUTCTime,getCurrentTime)

import qualified System.Log.Logger as Logger
import System.Log.Logger (Priority(DEBUG),rootLoggerName,setLevel,updateGlobalLogger)
import System.Log.Logger.TH

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Path,Test)
import Test.QuickCheck.Instances ()

import Text.Printf (printf)

import qualified LogicGrowsOnTrees.Parallel.Adapter.Threads as Threads
import qualified LogicGrowsOnTrees.Parallel.Common.Workgroup as Workgroup
import LogicGrowsOnTrees.Parallel.ExplorationMode
import LogicGrowsOnTrees.Parallel.Common.RequestQueue hiding (addWorkerCountListener,setWorkloadBufferSize)
import LogicGrowsOnTrees.Testing

deriveLoggers "Logger" [DEBUG]

main :: IO ()
main = do
    -- updateGlobalLogger rootLoggerName (setLevel DEBUG)
    defaultMain [tests]

data CPUTimeTrackerTestResults = CPUTimeTrackerTestResults
  { time_1_expected :: NominalDiffTime
  , time_1_actual :: NominalDiffTime
  , time_2_expected :: NominalDiffTime
  , time_2_actual :: NominalDiffTime
  , time_3_expected :: NominalDiffTime
  , time_3_actual :: NominalDiffTime
  , time_4_expected :: NominalDiffTime
  , time_4_actual :: NominalDiffTime
  , time_5_expected :: NominalDiffTime
  , time_5_actual :: NominalDiffTime
  }

tests :: Test
tests = testGroup "LogicGrowsOnTrees.Parallel.Common.RequestQueue"
    [testCase "CPU time tracker" $ do
        quit_var ← newEmptyMVar
        results_var ← newEmptyMVar
        let initial_time = 0.01
        tracker ← newCPUTimeTracker initial_time
        _ ← flip Threads.exploreTreeIO (liftIO $ takeMVar quit_var) $ do
            startCPUTimeTracker tracker
            startCPUTimeTracker tracker -- intentional for test purposes
            current_time_1 ← liftIO $ getCurrentTime
            time_1_actual ← liftIO $ getCurrentCPUTime tracker
            let time_1_expected = initial_time

            debugM "Test with one worker."
            Workgroup.setNumberOfWorkers 1
            liftIO $ threadDelay 100000
            current_time_2 ← liftIO $ getCurrentTime
            let diff_time_12 = current_time_2 `diffUTCTime` current_time_1
            time_2_actual ← liftIO $ getCurrentCPUTime tracker
            let time_2_expected = initial_time + diff_time_12

            debugM "Test with two workers."
            Workgroup.setNumberOfWorkers 2
            liftIO $ threadDelay 300000
            current_time_3 ← liftIO $ getCurrentTime
            let diff_time_23 = current_time_3 `diffUTCTime` current_time_2
            time_3_actual ← liftIO $ getCurrentCPUTime tracker
            let time_3_expected = initial_time + diff_time_12 + 2*diff_time_23

            debugM "Test with three workers."
            Workgroup.setNumberOfWorkers 3
            liftIO $ threadDelay 500000
            current_time_4 ← liftIO $ getCurrentTime
            let diff_time_34 = current_time_4 `diffUTCTime` current_time_3
            time_4_actual ← liftIO $ getCurrentCPUTime tracker
            let time_4_expected = initial_time + diff_time_12 + 2*diff_time_23 + 3*diff_time_34

            debugM "Test with one worker again."
            Workgroup.setNumberOfWorkers 1
            liftIO $ threadDelay 700000
            current_time_5 ← liftIO $ getCurrentTime
            let diff_time_45 = current_time_5 `diffUTCTime` current_time_4
            time_5_actual ← liftIO $ getCurrentCPUTime tracker
            let time_5_expected = initial_time + diff_time_12 + 2*diff_time_23 + 3*diff_time_34 + diff_time_45

            liftIO $ putMVar results_var CPUTimeTrackerTestResults{..}

            abort

        putMVar quit_var ()

        CPUTimeTrackerTestResults{..} ← takeMVar results_var
        let errorOf x y = abs (x-y) / (abs x + abs y) * 2
            error_1 = errorOf time_1_expected time_1_actual
            error_2 = errorOf time_2_expected time_2_actual
            error_3 = errorOf time_3_expected time_3_actual
            error_4 = errorOf time_4_expected time_4_actual
            error_5 = errorOf time_5_expected time_5_actual
            comparisonStringOf (n::Int) x y err = printf "%i %s vs. %s (%s%%)" n (show x) (show y) (show (err*100))
            comparison_string_1 = comparisonStringOf 1 time_1_expected time_1_actual error_1
            comparison_string_2 = comparisonStringOf 2 time_2_expected time_2_actual error_2
            comparison_string_3 = comparisonStringOf 3 time_3_expected time_3_actual error_3
            comparison_string_4 = comparisonStringOf 4 time_4_expected time_4_actual error_4
            comparison_string_5 = comparisonStringOf 5 time_5_expected time_5_actual error_5
        debugM comparison_string_1
        debugM comparison_string_2
        debugM comparison_string_3
        debugM comparison_string_4
        debugM comparison_string_5
        assertEqual comparison_string_1 time_1_expected time_1_actual
        assertBool comparison_string_2 (error_2 < 0.02)
        assertBool comparison_string_3 (error_3 < 0.01)
        assertBool comparison_string_4 (error_4 < 0.01)
        assertBool comparison_string_5 (error_5 < 0.001)
    ,testCase "kills all controller threads" $ do
        starts@[a,b,c,d] ← replicateM 4 newEmptyMVar
        vars@[w,x,y,z] ← replicateM 4 newEmptyMVar
        request_queue ← newRequestQueue
        forkControllerThread request_queue . liftIO $ do
            try (putMVar a () >> forever yield) >>= putMVar w
        forkControllerThread request_queue $ do
            void $ fork (do
                void $ fork . liftIO $ try (putMVar b () >> forever yield) >>= putMVar x
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
    ]
