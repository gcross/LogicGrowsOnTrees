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

import Control.Arrow ((&&&))
import Control.Concurrent
import Control.Exception (AsyncException(ThreadKilled))
import Control.Monad
import Control.Monad.Catch (catch)
import Control.Monad.IO.Class

import qualified Data.IntSet as IntSet
import Data.IORef
import Data.List (sort)
import qualified Data.Map as Map

import System.IO.Unsafe
import qualified System.Log.Logger as Logger
import System.Log.Logger (Priority(DEBUG),rootLoggerName,setLevel,updateGlobalLogger)
import System.Log.Logger.TH
import System.Random (randomRIO)

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit hiding (Path,Test)
import Test.QuickCheck.Instances ()

import LogicGrowsOnTrees
import LogicGrowsOnTrees.Checkpoint
import qualified LogicGrowsOnTrees.Parallel.Adapter.Threads as Threads
import qualified LogicGrowsOnTrees.Parallel.Common.Workgroup as Workgroup
import LogicGrowsOnTrees.Parallel.Main (RunOutcome(..),TerminationReason(..))
import LogicGrowsOnTrees.Parallel.Common.RequestQueue hiding (addWorkerCountListener,setWorkloadBufferSize)
import LogicGrowsOnTrees.Testing

deriveLoggers "Logger" [DEBUG]

main :: IO ()
main = do
    -- updateGlobalLogger rootLoggerName (setLevel DEBUG)
    defaultMain [tests]

tests :: Test
tests = testGroup "LogicGrowsOnTrees.Parallel.Adapter.Threads"
    [testGroup "FirstMode"
        [testCase "two threads, one blocked" $ do
            RunOutcome _ termination_reason ←
                Threads.exploreTreeIOUntilFirst
                    (Workgroup.setNumberOfWorkers 2)
                    (liftIO (threadDelay 1) >> endless_tree
                      `mplus`
                      return ()
                    )
            termination_reason @?= Completed (Just (Progress (ChoicePoint Unexplored Explored) ()))
        ]
    ,testGroup "FoundModeUsingPull"
        [testCase "many threads with combined final result but none finish" $ do
            RunOutcome _ termination_reason ←
                Threads.exploreTreeIOUntilFoundUsingPull
                    ((== 2) . length)
                    (Workgroup.setNumberOfWorkers 4)
                    ((return [1::Int] `mplus` endless_tree) `mplus` (return [2] `mplus` endless_tree))
            case termination_reason of
                Completed (Right (Progress _ result)) → sort result @?= [1,2]
                _ → fail $ "got incorrect result: " ++ show termination_reason
        ]
    ,testGroup "FoundModeUsingPush"
        [testCase "two threads with combined final result but none finish" $ do
            RunOutcome _ termination_reason ←
                Threads.exploreTreeIOUntilFoundUsingPush
                    ((== 2) . length)
                    (Workgroup.setNumberOfWorkers 2)
                    ((return [1::Int] `mplus` endless_tree) `mplus` (return [2] `mplus` endless_tree))
            case termination_reason of
                Completed (Right (Progress _ result)) → sort result @?= [1,2]
                _ → fail $ "got incorrect result: " ++ show termination_reason
        ]
    ,plusTestOptions (mempty {topt_maximum_generated_tests = Just 10}) $ testGroup "stress tests" $
        let extractResult (RunOutcome _ termination_reason) =
                case termination_reason of
                    Aborted _ → error "prematurely aborted"
                    Completed result → return result
                    Failure _ message → error message
            insertHooks cleared_flags_mvar request_queue = ($ \hook_id → liftIO $ do
                threadDelay 10
                mvar ← modifyMVar cleared_flags_mvar $ \cleared_flags →
                    case Map.lookup hook_id cleared_flags of
                        Nothing → do
                            mvar ← newEmptyMVar
                            writeChan request_queue mvar
                            return (Map.insert hook_id mvar cleared_flags,mvar)
                        Just mvar → return (cleared_flags,mvar)
                readMVar mvar
              )
            receiveProgressInto progresses_ref progress = atomicModifyIORef progresses_ref ((progress:) &&& const ())
            respondToRequests request_queue generateNoise progresses_ref = do
                Workgroup.setNumberOfWorkers 1
                forever $ do
                    liftIO $ threadDelay 10
                    mvar ← liftIO $ readChan request_queue
                    liftIO $ threadDelay 10
                    void $ generateNoise $ receiveProgressInto progresses_ref
                    liftIO $ threadDelay 10
                    liftIO $ putMVar mvar ()
            oneThreadNoise receiveProgress = liftIO (randomRIO (0,1::Int)) >>= \case
                0 → do Workgroup.setNumberOfWorkers 0
                       Workgroup.setNumberOfWorkers 1
                1 → void $ requestProgressUpdateAsync receiveProgress
                _ → error "RNG broke and gave us a number not 0 or 1"
            twoThreadsNoise receiveProgress = liftIO (randomRIO (0,1::Int)) >>= \case
                0 → void $ Workgroup.changeNumberOfWorkers (3-)
                1 → void $ requestProgressUpdateAsync receiveProgress
                _ → error "RNG broke and gave us a number not 0 or 1"
            manyThreadsNoise receiveProgress = liftIO (randomRIO (0,2::Int)) >>= \case
                0 → void $ Workgroup.changeNumberOfWorkers (\i → if i > 1 then i-1 else i)
                1 → void $ Workgroup.changeNumberOfWorkers (+1)
                2 → void $ requestProgressUpdateAsync receiveProgress
                _ → error "RNG broke and gave us a number not 0 or 1"
        in
        [testGroup "AllMode" $
            let runTest generateNoise = randomUniqueTreeWithHooks >>= \constructTree → return . unsafePerformIO $ do
                    cleared_flags_mvar ← newMVar mempty
                    request_queue ← newChan
                    progresses_ref ← newIORef []
                    result ←
                        (Threads.exploreTreeIO
                            (respondToRequests request_queue generateNoise progresses_ref)
                            (insertHooks cleared_flags_mvar request_queue constructTree)
                        ) >>= extractResult
                    let tree = constructTree (const $ return ())
                    correct_result ← exploreTreeT tree
                    result @?= correct_result
                    (remdups <$> readIORef progresses_ref) >>= mapM_ (\(Progress checkpoint result) → do
                        exploreTreeTStartingFromCheckpoint (invertCheckpoint checkpoint) tree >>= (@?= result)
                        exploreTreeTStartingFromCheckpoint checkpoint tree >>= (@?= correct_result ) . mappend result
                      )
                    return True
          in
          [testProperty "one thread" . runTest $ oneThreadNoise
          ,testProperty "two threads" . runTest $ twoThreadsNoise
          ,testProperty "many threads" . runTest $ manyThreadsNoise
          ]
        ,testGroup "FirstMode" $
            let runTest generator generateNoise = generator >>= \constructTree → return . unsafePerformIO $ do
                    cleared_flags_mvar ← newMVar mempty
                    request_queue ← newChan
                    progresses_ref ← newIORef []
                    maybe_result ←
                        (Threads.exploreTreeIOUntilFirst
                            (respondToRequests request_queue generateNoise progresses_ref)
                            (insertHooks cleared_flags_mvar request_queue constructTree)
                        ) >>= extractResult
                    let tree = constructTree (const $ return ())
                    correct_results ← exploreTreeT tree
                    case maybe_result of
                        Nothing → assertBool "solutions were missed" (IntSet.null correct_results)
                        Just (Progress checkpoint result) → do
                            IntSet.size result @?= 1
                            assertBool "solution was not valid" $ result `IntSet.isSubsetOf` correct_results
                            exploreTreeTStartingFromCheckpoint (invertCheckpoint checkpoint) tree >>= (@=? result)
                            exploreTreeTStartingFromCheckpoint checkpoint tree >>= (@=? IntSet.difference correct_results result)
                    (remdups <$> readIORef progresses_ref) >>= mapM_ (\checkpoint → do
                        exploreTreeTUntilFirstStartingFromCheckpoint (invertCheckpoint checkpoint) tree >>= (@?= Nothing)
                      )
                    return True
                testGroupUsingGenerator name generator = testGroup name $
                    [testProperty "one thread" . runTest generator $ oneThreadNoise
                    ,testProperty "two threads" . runTest generator $ twoThreadsNoise
                    ,testProperty "many threads" . runTest generator $ manyThreadsNoise
                    ]
            in [testGroupUsingGenerator "with solutions" randomUniqueTreeWithHooks
                ,testGroupUsingGenerator "without solutions" randomNullTreeWithHooks
                ]
        ,testGroup "FoundModeUsingPull" $
            let runTest generator generateNoise = generator >>= \constructTree → return . unsafePerformIO $ do
                    let tree = constructTree (const $ return ())
                    all_results ← exploreTreeT tree
                    number_of_results_to_find ← randomRIO (1,2*IntSet.size all_results)
                    cleared_flags_mvar ← newMVar mempty
                    request_queue ← newChan
                    progresses_ref ← newIORef []
                    result ←
                        (Threads.exploreTreeIOUntilFoundUsingPull
                            ((>= number_of_results_to_find) . IntSet.size)
                            (respondToRequests request_queue generateNoise progresses_ref)
                            (insertHooks cleared_flags_mvar request_queue constructTree)
                        ) >>= extractResult
                    case result of
                        Left incomplete_result → do
                            assertBool "result is not smaller than desired" $ IntSet.size incomplete_result < number_of_results_to_find
                            assertEqual "incomplete result matches all results" incomplete_result all_results
                        Right (Progress checkpoint final_result) → do
                            assertBool "final result is at least as large as desired" $ IntSet.size final_result >= number_of_results_to_find
                            assertBool "final result was not valid" $ final_result `IntSet.isSubsetOf` all_results
                            exploreTreeTStartingFromCheckpoint (invertCheckpoint checkpoint) tree
                                >>= assertEqual "final results together do not match all results covered by the checkpoint" final_result
                            exploreTreeTStartingFromCheckpoint checkpoint tree
                                >>= assertEqual "all results minus final results do not match remaining results" (IntSet.difference all_results final_result)
                    (remdups <$> readIORef progresses_ref) >>= mapM_ (\(Progress checkpoint result) → do
                        exploreTreeTStartingFromCheckpoint (invertCheckpoint checkpoint) tree >>= (@?= result)
                        exploreTreeTStartingFromCheckpoint checkpoint tree >>= (@?= all_results) . mappend result
                      )
                    return True
                testGroupUsingGenerator name generator = testGroup name $
                    [testProperty "one thread" . runTest generator $ oneThreadNoise
                    ,testProperty "two threads" . runTest generator $ twoThreadsNoise
                    ,testProperty "many threads" . runTest generator $ manyThreadsNoise
                    ]
            in [testGroupUsingGenerator "with solutions" randomUniqueTreeWithHooks
                ,testGroupUsingGenerator "without solutions" randomNullTreeWithHooks
                ]
        ,testGroup "FoundModeUsingPush" $
            let runTest generator generateNoise = generator >>= \constructTree → return . unsafePerformIO $ do
                    let tree = constructTree (const $ return ())
                    all_results ← exploreTreeT tree
                    number_of_results_to_find ← randomRIO (1,2*IntSet.size all_results)
                    cleared_flags_mvar ← newMVar mempty
                    request_queue ← newChan
                    progresses_ref ← newIORef []
                    result ←
                        (Threads.exploreTreeIOUntilFoundUsingPush
                            ((>= number_of_results_to_find) . IntSet.size)
                            (respondToRequests request_queue generateNoise progresses_ref)
                            (insertHooks cleared_flags_mvar request_queue constructTree)
                        ) >>= extractResult
                    case result of
                        Left incomplete_result → do
                            assertBool "result is not smaller than desired" $ IntSet.size incomplete_result < number_of_results_to_find
                            assertEqual "incomplete result matches all results" incomplete_result all_results
                        Right (Progress checkpoint final_result) → do
                            assertBool "result is at least as large as desired" $ IntSet.size final_result >= number_of_results_to_find
                            assertBool "final result was not valid" $ final_result `IntSet.isSubsetOf` all_results
                            exploreTreeTStartingFromCheckpoint (invertCheckpoint checkpoint) tree
                                >>= assertEqual "both returned results together do not match all results covered by the checkpoint" final_result
                            exploreTreeTStartingFromCheckpoint checkpoint tree
                                >>= assertEqual "all results minus return results do not match remaining results" (IntSet.difference all_results final_result)
                    (remdups <$> readIORef progresses_ref) >>= mapM_ (\(Progress checkpoint result) → do
                        exploreTreeTStartingFromCheckpoint (invertCheckpoint checkpoint) tree >>= (@?= result)
                        exploreTreeTStartingFromCheckpoint checkpoint tree >>= (@?= all_results) . mappend result
                      )
                    return True
                testGroupUsingGenerator name generator = testGroup name $
                    [testProperty "one thread" . runTest generator $ oneThreadNoise
                    ,testProperty "two threads" . runTest generator $ twoThreadsNoise
                    ,testProperty "many threads" . runTest generator $ manyThreadsNoise
                    ]
            in [testGroupUsingGenerator "with solutions" randomUniqueTreeWithHooks
                ,testGroupUsingGenerator "without solutions" randomNullTreeWithHooks
                ]
        ]
    ,testCase "processPendingRequests" $ do
        mvar ← newEmptyMVar
        RunOutcome{..} ← Threads.exploreTreeIO (Threads.setNumberOfWorkers 2) $
            let go = processPendingRequests >> liftIO (tryTakeMVar mvar) >>= maybe go return
            in go `mplus` liftIO (putMVar mvar ())
        case runTerminationReason of
            Aborted _ → error "aborted"
            Completed () → return ()
            Failure _ message → error message
        return ()
    ,testCase "threads are killed" $ do
        exception_1_mvar ← newEmptyMVar
        exception_2_mvar ← newEmptyMVar
        _ ← Threads.exploreTree
             ((do _ ← Threads.fork $
                    (do Threads.setNumberOfWorkers 1
                        liftIO . forever $ threadDelay 1000000
                    )
                    `catch`
                    (\(x::AsyncException) → liftIO $ putMVar exception_2_mvar x)
                  liftIO . forever $ threadDelay 1000000
              )
              `catch`
              (\(x::AsyncException) → liftIO $ putMVar exception_1_mvar x)
            )
            (return ())
        takeMVar exception_1_mvar >>= (@?= ThreadKilled)
        takeMVar exception_2_mvar >>= (@?= ThreadKilled)
    ]
