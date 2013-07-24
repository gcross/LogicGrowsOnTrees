{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

{-| This adapter implements parallelism by spawning multiple processes.  The
    number of processes can be changed during the run and even be set to zero.
 -}
module LogicGrowsOnTrees.Parallel.Adapter.Processes
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
    -- * Generic runner functions
    -- $runners
    , runSupervisor
    , runExplorer
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

import LogicGrowsOnTrees (Tree,TreeIO,TreeT)
import LogicGrowsOnTrees.Checkpoint
import LogicGrowsOnTrees.Parallel.Common.ExplorationMode
import LogicGrowsOnTrees.Parallel.Common.Message
import qualified LogicGrowsOnTrees.Parallel.Common.Process as Process
import LogicGrowsOnTrees.Parallel.Common.Process
import LogicGrowsOnTrees.Parallel.Common.Purity
import LogicGrowsOnTrees.Parallel.Common.Supervisor.RequestQueue
import LogicGrowsOnTrees.Parallel.Common.Worker as Worker hiding (ProgressUpdate,StolenWorkload,exploreTree,exploreTreeIO,exploreTreeT)
import LogicGrowsOnTrees.Parallel.Common.Workgroup hiding (C,unwrapC)
import LogicGrowsOnTrees.Parallel.Main (Driver(..),DriverParameters(..),RunOutcome,RunOutcomeFor,mainParser)
import LogicGrowsOnTrees.Utils.Handle
import LogicGrowsOnTrees.Workload

--------------------------------------------------------------------------------
----------------------------------- Loggers ------------------------------------
--------------------------------------------------------------------------------

deriveLoggers "Logger" [DEBUG,INFO,ERROR]

--------------------------------------------------------------------------------
------------------------------------ Driver ------------------------------------
--------------------------------------------------------------------------------

{-| This is the driver for the threads adapter.  The number of workers is
    specified via. the (required) command-line option "-n".

    Note that there are not seperate drivers for the supervisor process and the
    worker process;  instead, the same executable is used for both the
    supervisor and the worker, with a sentinel argument list determining which
    mode it should run in.
 -}
driver ::
    ( Serialize shared_configuration
    , Serialize (ProgressFor exploration_mode)
    , Serialize (WorkerFinalProgressFor exploration_mode)
    ) ⇒
    Driver IO shared_configuration supervisor_configuration m n exploration_mode
driver = Driver $ \DriverParameters{..} → do
    runExplorer
        constructExplorationMode
        purity
        (mainParser (liftA2 (,) shared_configuration_term (liftA2 (,) number_of_processes_term supervisor_configuration_term)) program_info)
        initializeGlobalState
        constructTree
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

{-| This is the monad in which the processes controller will run. -}
newtype ProcessesControllerMonad exploration_mode α =
    C { unwrapC :: WorkgroupControllerMonad (IntMap Worker) exploration_mode α
    } deriving (Applicative,Functor,Monad,MonadCatchIO,MonadIO,RequestQueueMonad,WorkgroupRequestQueueMonad)

instance HasExplorationMode (ProcessesControllerMonad exploration_mode) where
    type ExplorationModeFor (ProcessesControllerMonad exploration_mode) = exploration_mode

--------------------------------------------------------------------------------
------------------------------- Generic runners --------------------------------
--------------------------------------------------------------------------------

{- $runners
In this section the full functionality of this module is exposed in case one
does not want the restrictions of the driver interface. If you decide to go in
this direction, then you need to decide whether you want there to be a single
executable for both the supervisor and worker with the process of determining in
which mode it should run taken care of for you, or whether you want to manually
solve this problem in order to give yourself more control (such as by having
separate supervisor and worker executables) at the price of more work.

If you want to use a single executable with automated handling of the
supervisor and worker roles, then use 'runExplorer'.  Otherwise, use
'runSupervisor' to run the supervisor loop and on each worker use
'runWorkerUsingHandles', passing 'stdin' and 'stdout' as the process handles.
 -}

{-| This runs the supervisor, which will spawn and kill worker processes as
    needed so that the total number is equal to the number set by the
    controller.
 -}
runSupervisor ::
    ( Serialize (ProgressFor exploration_mode)
    , Serialize (WorkerFinalProgressFor exploration_mode)
    ) ⇒
    ExplorationMode exploration_mode {-^ the exploration mode -} →
    String {-^ the path to the worker executable -} →
    [String] {-^ the arguments to pass to the worker executable -} →
    (Handle → IO ()) {-^ an action that writes any information needed by the worker to the given handle -} →
    ProgressFor exploration_mode {-^ the initial progress of the run -} →
    ProcessesControllerMonad exploration_mode () {-^ the controller of the supervisor, which must at least set the number of workers to be positive for anything to take place -} →
    IO (RunOutcomeFor exploration_mode) {-^ the result of the run -}
runSupervisor
    exploration_mode
    worker_filepath
    worker_arguments
    sendConfigurationTo
    starting_progress
    (C controller)
 = do
    request_queue ← newRequestQueue
    runWorkgroup
        exploration_mode
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

{-| Explores the given tree using multiple processes to achieve parallelism.

    This function grants access to all of the functionality of this adapter,
    rather than having to go through the more restricted driver interface. The
    signature of this function is very complicated because it is meant to be
    used in both the supervisor and worker;  it figures out which role it is
    supposed to play based on whether the list of command line arguments matches
    a sentinel.

    The configuration information is divided into two parts: information shared
    between the supervisor and the workers, and information that is specific to
    the supervisor and not sent to the workers. (Note that only the former needs
    to be serializable.) An action must be supplied that obtains this
    configuration information, and most of the arguments are functions that are
    given all or part of this information.
 -}
runExplorer ::
    ( Serialize shared_configuration
    , Serialize (ProgressFor exploration_mode)
    , Serialize (WorkerFinalProgressFor exploration_mode)
    ) ⇒
    (shared_configuration → ExplorationMode exploration_mode) {-^ construct the exploration mode given the shared configuration -} →
    Purity m n {-^ the purity of the tree -} →
    IO (shared_configuration,supervisor_configuration) {-^ get the shared and supervisor-specific configuration information (run only on the supervisor) -} →
    (shared_configuration → IO ()) {-^ initialize the global state of the process given the shared configuration (run on both supervisor and worker processes) -} →
    (shared_configuration → TreeT m (ResultFor exploration_mode)) {-^ construct the tree from the shared configuration (run only on the worker) -} →
    (shared_configuration → supervisor_configuration → IO (ProgressFor exploration_mode)) {-^ get the starting progress given the full configuration information (run only on the supervisor) -} →
    (shared_configuration → supervisor_configuration → ProcessesControllerMonad exploration_mode ()) {-^ construct the controller for the supervisor, which must at least set the number of workers to be non-zero (run only on the supervisor) -} →
    IO (Maybe ((shared_configuration,supervisor_configuration),RunOutcomeFor exploration_mode))
        {-^ if this process is the supervisor, then returns the outcome of the
            run as well as the configuration information wrapped in 'Just';
            otherwise, if this process is a worker, it returns 'Nothing'
         -}
runExplorer
    constructExplorationMode
    purity
    getConfiguration
    initializeGlobalState
    constructTree
    getStartingProgress
    constructManager
  = getArgs >>= \args →
    if args == sentinel
        then do
            shared_configuration ← receive stdin
            initializeGlobalState shared_configuration
            runWorkerUsingHandles
                (constructExplorationMode shared_configuration)
                purity
                (constructTree shared_configuration)
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
                    (constructExplorationMode shared_configuration)
                    program_filepath
                    sentinel
                    (flip send shared_configuration)
                    starting_progress
                    (constructManager shared_configuration supervisor_configuration)
            return $ Just (configuration,termination_result)
  where
    sentinel = ["explorer","worker","bee"]

--------------------------------------------------------------------------------
------------------------------- Utility funtions -------------------------------
--------------------------------------------------------------------------------

{-| Gets the full path to this executable. -}
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
