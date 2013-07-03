{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

{-| This back-end implements parallelism by spawning multiple threads.  The
    number of threads can be changed during the run and even be set to zero.

    The driver provided by this back-end sets the number of processes equal to
    the value specified by the used via. the (required) "-n" option.
 -}
module Visitor.Parallel.BackEnd.Processes
    (
    -- * Driver
      driver
    -- * Controller
    , ProcessesControllerMonad
    , abort
    , changeNumberOfWorkers
    , changeNumberOfWorkersAsync
    , fork
    , getCurrentProgress
    , getCurrentProgressAsync
    , getNumberOfWorkers
    , getNumberOfWorkersAsync
    , requestProgressUpdate
    , requestProgressUpdateAsync
    -- * Visit functions
    , runSupervisor
    , runVisitor
    -- * Utility functions
    , getProgFilepath
    ) where

import Prelude hiding (catch)

import Control.Applicative ((<$>),(<*>),Applicative,liftA2)
import Control.Arrow (second)
import Control.Concurrent (ThreadId,forkIO,getNumCapabilities,killThread)
import Control.Exception (AsyncException(ThreadKilled,UserInterrupt),SomeException,catch,catchJust,fromException)
import Control.Monad (forever,liftM2,unless,void)
import Control.Monad.CatchIO (MonadCatchIO)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Control.Monad.Trans.State.Strict (get,modify)

import qualified Data.Foldable as Fold
import Data.Function (fix)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe (fromJust,fromMaybe)
import Data.Monoid (Monoid(mempty))
import Data.Serialize (Serialize)

import System.Console.CmdTheLine
import System.Environment (getArgs,getProgName)
import System.Environment.FindBin (getProgPath)
import System.FilePath ((</>))
import System.IO (Handle,hGetLine,stdin,stdout)
import System.IO.Error (isEOFError)
import qualified System.Log.Logger as Logger
import System.Log.Logger (Priority(DEBUG,INFO,ERROR))
import System.Log.Logger.TH
import System.Process (CreateProcess(..),CmdSpec(RawCommand),StdStream(..),ProcessHandle,createProcess,interruptProcessGroupOf)

import Visitor (TreeGenerator,TreeGeneratorIO,TreeGeneratorT)
import Visitor.Checkpoint
import Visitor.Parallel.Main (Driver(..),DriverParameters(..),RunOutcome,RunOutcomeFor,mainParser)
import Visitor.Parallel.Common.Message
import qualified Visitor.Parallel.Common.Process as Process
import Visitor.Parallel.Common.Process
import Visitor.Parallel.Common.Supervisor.RequestQueue
import Visitor.Parallel.Common.Worker as Worker hiding (ProgressUpdate,StolenWorkload,visitTree,visitTreeIO,visitTreeT)
import Visitor.Parallel.Common.VisitorMode
import Visitor.Parallel.Common.Workgroup hiding (C,unwrapC)
import Visitor.Utils.Handle
import Visitor.Workload

--------------------------------------------------------------------------------
----------------------------------- Loggers ------------------------------------
--------------------------------------------------------------------------------

deriveLoggers "Logger" [DEBUG,INFO,ERROR]

--------------------------------------------------------------------------------
------------------------------------ Driver ------------------------------------
--------------------------------------------------------------------------------

driver ::
    ( Serialize shared_configuration
    , Serialize (ProgressFor visitor_mode)
    , Serialize (WorkerFinalProgressFor visitor_mode)
    ) ⇒
    Driver IO shared_configuration supervisor_configuration m n visitor_mode
driver = Driver $ \DriverParameters{..} → do
    runVisitor
        constructVisitorMode
        purity
        (mainParser (liftA2 (,) shared_configuration_term (liftA2 (,) number_of_processes_term supervisor_configuration_term)) program_info)
        initializeGlobalState
        constructTreeGenerator
        (curry $ uncurry getStartingProgress . second snd)
        (\shared_configuration (number_of_processes,supervisor_configuration) → do
            changeNumberOfWorkers (const $ return number_of_processes)
            constructManager shared_configuration supervisor_configuration
        )
    >>=
    maybe (return ()) (notifyTerminated <$> fst . fst <*> snd . snd . fst <*> snd)
  where
    number_of_processes_term = required (flip opt (
        (optInfo ["n","number-of-processes"])
        {   optName = "#"
        ,   optDoc = "This *required* option specifies the number of worker processes to spawn."
        }
        ) Nothing )

--------------------------------------------------------------------------------
---------------------------------- Controller ----------------------------------
--------------------------------------------------------------------------------

newtype ProcessesControllerMonad visitor_mode α =
    C { unwrapC :: WorkgroupControllerMonad (IntMap Worker) visitor_mode α
    } deriving (Applicative,Functor,Monad,MonadCatchIO,MonadIO,RequestQueueMonad,WorkgroupRequestQueueMonad)

instance HasVisitorMode (ProcessesControllerMonad visitor_mode) where
    type VisitorModeFor (ProcessesControllerMonad visitor_mode) = visitor_mode
                     
--------------------------------------------------------------------------------
------------------------------- Generic runners --------------------------------
--------------------------------------------------------------------------------

runSupervisor ::
    ( Serialize (ProgressFor visitor_mode)
    , Serialize (WorkerFinalProgressFor visitor_mode)
    ) ⇒
    VisitorMode visitor_mode {-^ the visitor mode -} →
    String →
    [String] →
    (Handle → IO ()) →
    ProgressFor visitor_mode →
    ProcessesControllerMonad visitor_mode () →
    IO (RunOutcomeFor visitor_mode)
runSupervisor
    visitor_mode
    worker_filepath
    worker_arguments
    sendConfigurationTo
    starting_progress
    (C controller)
 = do
    request_queue ← newRequestQueue
    runWorkgroup
        visitor_mode
        mempty
        (\message_receivers@MessageForSupervisorReceivers{..} →
            let createWorker worker_id = do
                    debugM $ "Launching worker process: " ++ worker_filepath ++ " " ++ unwords worker_arguments
                    (Just write_handle,Just read_handle,Just error_handle,process_handle) ← liftIO . createProcess $
                        CreateProcess
                        {   cmdspec = RawCommand worker_filepath worker_arguments
                        ,   cwd = Nothing
                        ,   env = Nothing
                        ,   std_in = CreatePipe
                        ,   std_out = CreatePipe
                        ,   std_err = CreatePipe
                        ,   close_fds = True
                        ,   create_group = True
                        }
                    liftIO $ do
                        _ ← forkIO $
                            catchJust
                                (\e → if isEOFError e then Just () else Nothing)
                                (forever $ hGetLine error_handle >>= \line → debugM $ "[" ++ show worker_id ++ "] " ++ line)
                                (const $ return ())
                             `catch`
                                (\(e::SomeException) → errorM $ "Error reading stderr for worker " ++ show worker_id ++ ": " ++ show e)
                        _ ← forkIO $
                            receiveAndProcessMessagesFromWorkerUsingHandle message_receivers read_handle worker_id
                            `catch`
                            (\(e::SomeException) →
                            case fromException e of
                                Just ThreadKilled → return ()
                                Just UserInterrupt → return ()
                                _ → do
                                    debugM $ "Worker " ++ show worker_id ++ " failed with exception: " ++ show e
                                    interruptProcessGroupOf process_handle
                                    receiveFailureFromWorker worker_id (show e)
                            )
                        sendConfigurationTo write_handle
                    modify . IntMap.insert worker_id $ Worker{..}

                destroyWorker worker_id worker_is_active = do
                    debugM $ "Sending QuitWorker to " ++ show worker_id ++ "..."
                    get >>=
                        liftIO
                        .
                        flip send QuitWorker
                        .
                        write_handle
                        .
                        fromJustOrBust ("destroyWorker failed to get record for " ++ show worker_id)
                        .
                        IntMap.lookup worker_id
                    debugM $ "Finished sending QuitWorker to " ++ show worker_id ++ "."
                    modify $ IntMap.delete worker_id

                killAllWorkers _ =
                    debugM "Killing all workers..." >>
                    get >>= liftIO . Fold.mapM_ (flip send QuitWorker . write_handle) >>
                    debugM "Done killing all workers."

                sendMessageToWorker message worker_id =
                    get >>=
                        liftIO
                        .
                        maybe (return ()) (
                            flip send message
                            .
                            write_handle
                        )
                        .
                        IntMap.lookup worker_id

                sendProgressUpdateRequestTo = sendMessageToWorker RequestProgressUpdate
                sendWorkloadStealRequestTo = sendMessageToWorker RequestWorkloadSteal
                sendWorkloadTo worker_id workload = sendMessageToWorker (StartWorkload workload) worker_id
            in WorkgroupCallbacks{..}
        )
        starting_progress
        controller

runVisitor ::
    ( Serialize shared_configuration
    , Serialize (ProgressFor visitor_mode)
    , Serialize (WorkerFinalProgressFor visitor_mode)
    ) ⇒
    (shared_configuration → VisitorMode visitor_mode) →
    Purity m n {-^ the purity of the tree generator -} →
    IO (shared_configuration,supervisor_configuration) →
    (shared_configuration → IO ()) →
    (shared_configuration → TreeGeneratorT m (ResultFor visitor_mode)) →
    (shared_configuration → supervisor_configuration → IO (ProgressFor visitor_mode)) →
    (shared_configuration → supervisor_configuration → ProcessesControllerMonad visitor_mode ()) →
    IO (Maybe ((shared_configuration,supervisor_configuration),RunOutcomeFor visitor_mode))
runVisitor
    constructVisitorMode
    purity
    getConfiguration
    initializeGlobalState
    constructTreeGenerator
    getStartingProgress
    constructManager
  = getArgs >>= \args →
    if args == sentinel
        then do
            shared_configuration ← receive stdin
            initializeGlobalState shared_configuration
            runWorkerUsingHandles
                (constructVisitorMode shared_configuration)
                purity
                (constructTreeGenerator shared_configuration)
                stdin
                stdout
            return Nothing
        else do
            configuration@(shared_configuration,supervisor_configuration) ← getConfiguration
            initializeGlobalState shared_configuration
            program_filepath ← getProgFilepath
            starting_progress ← getStartingProgress shared_configuration supervisor_configuration
            termination_result ←
                runSupervisor
                    (constructVisitorMode shared_configuration)
                    program_filepath
                    sentinel
                    (flip send shared_configuration)
                    starting_progress
                    (constructManager shared_configuration supervisor_configuration)
            return $ Just (configuration,termination_result)
  where
    sentinel = ["visitor-worker-bee"]

--------------------------------------------------------------------------------
------------------------------- Utility funtions -------------------------------
--------------------------------------------------------------------------------

getProgFilepath :: IO String
getProgFilepath = liftM2 (</>) getProgPath getProgName

--------------------------------------------------------------------------------
----------------------------------- Internal -----------------------------------
--------------------------------------------------------------------------------

data Worker = Worker
    {   read_handle :: Handle
    ,   write_handle :: Handle
    ,   process_handle :: ProcessHandle
    }

fromJustOrBust message = fromMaybe (error message)
