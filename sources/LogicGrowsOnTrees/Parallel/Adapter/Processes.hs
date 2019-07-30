{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
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
    , changeNumberOfWorkersAsync
    , changeNumberOfWorkers
    , fork
    , getCurrentProgressAsync
    , getCurrentProgress
    , getCurrentStatisticsAsync
    , getCurrentStatistics
    , getNumberOfWorkersAsync
    , getNumberOfWorkers
    , requestProgressUpdateAsync
    , requestProgressUpdate
    , setNumberOfWorkersAsync
    , setNumberOfWorkers
    , setWorkloadBufferSize
    -- * Outcome types
    , RunOutcome
    , RunStatistics(..)
    , TerminationReason(..)
    -- * Generic runner functions
    -- $runners
    , runSupervisor
    , runWorker
    , runWorkerUsingHandles
    , runExplorer
    -- * Utility functions
    , getProgFilepath
    ) where

import Control.Applicative ((<$>),(<*>),(<**>),Applicative)
import Control.Concurrent (forkIO)
import Control.Exception (AsyncException(ThreadKilled,UserInterrupt),SomeException,catch,catchJust,fromException)
import Control.Monad (forever,liftM2)
import Control.Monad.Catch (MonadCatch,MonadMask,MonadThrow)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Control.Monad.Trans.State.Strict (get,modify)

import qualified Data.Foldable as Fold
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(mempty))
import Data.Serialize (Serialize)

import Options.Applicative
    ( ParserInfo
    , auto
    , execParser
    , help
    , helper
    , info
    , long
    , metavar
    , option
    , short
    , showDefault
    , value
    )

import System.Environment (getArgs,getProgName)
import System.Environment.FindBin (getProgPath)
import System.FilePath ((</>))
import System.IO (Handle,hGetLine,stdin,stdout)
import System.IO.Error (isEOFError)
import qualified System.Log.Logger as Logger
import System.Log.Logger (Priority(DEBUG,ERROR))
import System.Log.Logger.TH
import System.Process
    ( CreateProcess(..)
    , CmdSpec(RawCommand)
    , StdStream(..)
    , ProcessHandle
    , createProcess
    , interruptProcessGroupOf
    )

import LogicGrowsOnTrees (TreeT)
import LogicGrowsOnTrees.Parallel.Common.Message
import LogicGrowsOnTrees.Parallel.Common.Process (runWorker,runWorkerUsingHandles)
import LogicGrowsOnTrees.Parallel.Common.RequestQueue
import LogicGrowsOnTrees.Parallel.Common.Workgroup hiding (C,unwrapC)
import LogicGrowsOnTrees.Parallel.ExplorationMode
import LogicGrowsOnTrees.Parallel.Main
    ( Driver(..)
    , DriverParameters(..)
    , RunOutcome
    , RunOutcomeFor
    , RunStatistics(..)
    , TerminationReason(..)
    )
import LogicGrowsOnTrees.Parallel.Purity
import LogicGrowsOnTrees.Utils.Handle

--------------------------------------------------------------------------------
----------------------------------- Loggers ------------------------------------
--------------------------------------------------------------------------------

deriveLoggers "Logger" [DEBUG,ERROR]

--------------------------------------------------------------------------------
------------------------------------- Types ------------------------------------
--------------------------------------------------------------------------------

data Configuration shared_configuration supervisor_configuration = Configuration
    { shared_configuration :: shared_configuration
    , supervisor_configuration :: supervisor_configuration
    , number_of_processes :: Word
    }

--------------------------------------------------------------------------------
------------------------------------ Driver ------------------------------------
--------------------------------------------------------------------------------

{-| This is the driver for the threads adapter; the number of workers is
    specified via. the (required) command-line option "-n".

    Note that there are not seperate drivers for the supervisor process and the
    worker process; instead, the same executable is used for both the supervisor
    and the worker, with a sentinel argument (or arguments) determining which
    role it should run as.
 -}
driver ::
    ( Serialize shared_configuration
    , Serialize (ProgressFor exploration_mode)
    , Serialize (WorkerFinishedProgressFor exploration_mode)
    ) ⇒
    Driver IO shared_configuration supervisor_configuration m n exploration_mode
driver = Driver $ \DriverParameters{..} → do
    let configuration_parser_info = flip info program_info $
            (Configuration
                 <$> shared_configuration_parser
                 <*> supervisor_configuration_parser
                 <*> number_of_processes_parser
            )
            <**>
            helper
    runExplorer
        constructExplorationMode
        purity
        configuration_parser_info
        initializeGlobalState
        constructTree
        getStartingProgress
        (\supervisor_configuration number_of_processes → do
            setNumberOfWorkers number_of_processes
            constructController supervisor_configuration
        )
    >>=
    maybe
        (pure ())
        (\(Configuration{..}, run_outcome) →
            notifyTerminated shared_configuration supervisor_configuration run_outcome
        )
  where
    number_of_processes_parser = option auto $ mconcat
        [ short 'n'
        , long "number-of-processes"
        , help "This option specifies the number of worker processes to spawn."
        , metavar "#"
        , value 1
        , showDefault
        ]
{-# INLINE driver #-}

--------------------------------------------------------------------------------
---------------------------------- Controller ----------------------------------
--------------------------------------------------------------------------------

{-| The monad in which the processes controller will run. -}
newtype ProcessesControllerMonad exploration_mode α =
    C (WorkgroupControllerMonad (IntMap Worker) exploration_mode α)
      deriving
        ( Applicative
        , Functor
        , Monad
        , MonadCatch
        , MonadIO
        , MonadMask
        , MonadThrow
        , RequestQueueMonad
        , WorkgroupRequestQueueMonad
        )

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
which mode it should run taken care of for you, or whether you want to do this
yourself in order to give yourself more control (such as by having separate
supervisor and worker executables) at the price of more work.

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
    , Serialize (WorkerFinishedProgressFor exploration_mode)
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
                        ,   delegate_ctlc = False
                        ,   detach_console = True
                        ,   create_new_console = False
                        ,   new_session = False
                        ,   child_group = Nothing
                        ,   child_user = Nothing
                        ,   use_process_jobs = False
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

                destroyWorker worker_id _ = do
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
{-# INLINE runSupervisor #-}

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
    , Serialize (WorkerFinishedProgressFor exploration_mode)
    ) ⇒
    (shared_configuration → ExplorationMode exploration_mode)
        {-^ a function that constructs the exploration mode given the shared
            configuration
         -} →
    Purity m n {-^ the purity of the tree -} →
    ParserInfo (Configuration shared_configuration supervisor_configuration)
        {-^ an action that gets the shared and supervisor-specific configuration
            information (run only on the supervisor)
         -} →
    (shared_configuration → IO ())
        {-^ an action that initializes the global state of the process given the
            shared configuration (run on both supervisor and worker processes)
         -} →
    (shared_configuration → TreeT m (ResultFor exploration_mode))
        {-^ a function that constructs the tree from the shared configuration
            (called only on the worker)
         -} →
    (shared_configuration → supervisor_configuration → IO (ProgressFor exploration_mode))
        {-^ an action that gets the starting progress given the full
            configuration information (run only on the supervisor)
         -} →
    (supervisor_configuration → Word → ProcessesControllerMonad exploration_mode ())
        {-^ a function that constructs the controller for the supervisor, which
            must at least set the number of workers to be non-zero (called only
            on the supervisor)
         -} →
    IO (Maybe (Configuration shared_configuration supervisor_configuration,RunOutcomeFor exploration_mode))
        {-^ if this process is the supervisor, then the outcome of the run as
            well as the configuration information wrapped in 'Just'; otherwise
            'Nothing'
         -}
runExplorer
    constructExplorationMode
    purity
    configuration_parser_info
    initializeGlobalState
    constructTree
    getStartingProgress
    constructController
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
            configuration@Configuration{..} ← execParser configuration_parser_info
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
                    (constructController supervisor_configuration number_of_processes)
            return $ Just (configuration,termination_result)
  where
    sentinel = ["explorer","worker","bee"]
{-# INLINE runExplorer #-}

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

fromJustOrBust :: String → Maybe α → α
fromJustOrBust message = fromMaybe (error message)
