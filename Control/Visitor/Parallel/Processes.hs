-- Language extensions {{{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Control.Visitor.Parallel.Processes -- {{{
    ( ProcessesControllerMonad
    , abort
    , changeNumberOfWorkers
    , changeNumberOfWorkersAsync
    , driver
    , fork
    , getCurrentProgress
    , getCurrentProgressAsync
    , getNumberOfWorkers
    , getNumberOfWorkersAsync
    , getProgFilepath
    , requestProgressUpdate
    , requestProgressUpdateAsync
    , runSupervisor
    , runVisitor
    , runVisitorIO
    , runVisitorT
    , runWorkerWithVisitor
    , runWorkerWithVisitorIO
    , runWorkerWithVisitorT
    ) where -- }}}

-- Imports {{{
import Prelude hiding (catch)

import Control.Applicative (Applicative,liftA2)
import Control.Concurrent (ThreadId,forkIO,getNumCapabilities,killThread)
import Control.Exception (AsyncException(ThreadKilled,UserInterrupt),SomeException,catch,catchJust,fromException)
import Control.Monad (forever,liftM2,unless,void)
import Control.Monad.CatchIO (MonadCatchIO)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Control.Monad.Trans.State.Strict (get,modify)

import qualified Data.ByteString as BS
import Data.ByteString (hGet,hPut)
import qualified Data.Foldable as Fold
import Data.Function (fix)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe (fromJust,fromMaybe)
import Data.Monoid (Monoid(mempty))
import Data.Serialize (Serialize,encode,decode)

import System.Console.CmdTheLine
import System.Environment (getArgs,getProgName)
import System.Environment.FindBin (getProgPath)
import System.FilePath ((</>))
import System.IO (Handle,hFlush,hGetLine,hPrint,stdin,stdout)
import System.IO.Error (isEOFError)
import qualified System.Log.Logger as Logger
import System.Log.Logger (Priority(DEBUG,INFO,ERROR))
import System.Log.Logger.TH
import System.Process (CreateProcess(..),CmdSpec(RawCommand),StdStream(..),ProcessHandle,createProcess,interruptProcessGroupOf)

import Control.Visitor (Visitor,VisitorIO,VisitorT)
import Control.Visitor.Checkpoint
import Control.Visitor.Main (Driver(Driver),RunOutcome,mainParser)
import qualified Control.Visitor.Parallel.Process as Process
import Control.Visitor.Parallel.Process
import Control.Visitor.Parallel.Workgroup
import Control.Visitor.Supervisor.RequestQueue
import Control.Visitor.Worker as Worker hiding (ProgressUpdate,StolenWorkload,runVisitor,runVisitorIO,runVisitorT)
import Control.Visitor.Workload
-- }}}

-- Logging Functions {{{
deriveLoggers "Logger" [DEBUG,INFO,ERROR]
-- }}}

-- Types {{{

data Worker = Worker -- {{{
    {   read_handle :: Handle
    ,   write_handle :: Handle
    ,   process_handle :: ProcessHandle
    }
-- }}}

newtype ProcessesControllerMonad result α = C { unwrapC :: WorkgroupControllerMonad (IntMap Worker) result α} deriving (Applicative,Functor,Monad,MonadCatchIO,MonadIO,WorkgroupRequestQueueMonad)

-- }}}

-- Instances {{{
instance RequestQueueMonad (ProcessesControllerMonad result) where
    type RequestQueueMonadResult (ProcessesControllerMonad result) = result
    abort = C abort
    fork = C . fork . unwrapC
    getCurrentProgressAsync = C . getCurrentProgressAsync
    getNumberOfWorkersAsync = C . getNumberOfWorkersAsync
    requestProgressUpdateAsync = C . requestProgressUpdateAsync
-- }}}

-- Drivers {{{
driver :: Serialize configuration ⇒ Driver IO configuration visitor result -- {{{
driver = Driver $ \forkVisitorWorkerThread configuration_term term_info initializeGlobalState getMaybeStartingProgress notifyTerminated constructVisitor constructManager →
    genericRunVisitor
        forkVisitorWorkerThread
        (mainParser (liftA2 (,) number_of_processes_term configuration_term) term_info)
        (initializeGlobalState . snd)
        (getMaybeStartingProgress . snd)
        (\(number_of_processes,configuration) → do
            changeNumberOfWorkers (const $ return number_of_processes)
            constructManager configuration
        )
        (constructVisitor . snd)
    >>=
    maybe (return ()) (uncurry $ notifyTerminated . snd)
  where
    number_of_processes_term = required (flip opt (
        (optInfo ["n","number-of-processes"])
        {   optName = "#"
        ,   optDoc = "This *required* option specifies the number of worker processes to spawn."
        }
        ) Nothing )
-- }}}
-- }}}

-- Exposed Functions {{{

getProgFilepath :: IO String -- {{{
getProgFilepath = liftM2 (</>) getProgPath getProgName
-- }}}

runSupervisor :: -- {{{
    (Monoid result, Serialize result) ⇒
    String →
    [String] →
    (Handle → IO ()) →
    Maybe (Progress result) →
    ProcessesControllerMonad result () →
    IO (RunOutcome result)
runSupervisor worker_filepath worker_arguments sendConfigurationTo maybe_starting_progress (C controller) = do
    request_queue ← newRequestQueue
    runWorkgroup
        mempty
        (\WorkgroupReceivers{..} → -- {{{
            let createWorker worker_id = do -- {{{
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
                        _ ← forkIO $ (
                            (fix $ \receiveNextMessage → receive read_handle >>= (\message →
                                case message of
                                    Failed message → do
                                        receiveFailureFromWorker worker_id message
                                        receiveNextMessage
                                    Finished final_progress → do
                                        receiveFinishedFromWorker worker_id final_progress
                                        receiveNextMessage
                                    ProgressUpdate progress_update → do
                                        receiveProgressUpdateFromWorker worker_id progress_update
                                        receiveNextMessage
                                    StolenWorkload stolen_workload → do
                                        receiveStolenWorkloadFromWorker worker_id stolen_workload
                                        receiveNextMessage
                                    WorkerQuit →
                                        receiveQuitFromWorker worker_id
                            ))
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
                         )
                        sendConfigurationTo write_handle
                    modify . IntMap.insert worker_id $ Worker{..}
                -- }}}
                destroyWorker worker_id worker_is_active = do -- {{{
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
                -- }}}
                killAllWorkers _ = -- {{{
                    debugM "Killing all workers..." >>
                    get >>= liftIO . Fold.mapM_ (flip send QuitWorker . write_handle) >>
                    debugM "Done killing all workers."
                -- }}}
                sendMessageToWorker message worker_id = -- {{{
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
                -- }}}
                sendProgressUpdateRequestTo = sendMessageToWorker RequestProgressUpdate
                sendWorkloadStealRequestTo = sendMessageToWorker RequestWorkloadSteal
                sendWorkloadTo worker_id workload = sendMessageToWorker (StartWorkload workload) worker_id
            in WorkgroupCallbacks{..}
        ) -- }}}
        maybe_starting_progress
        controller
-- }}}

runVisitor :: -- {{{
    (Serialize configuration, Monoid result, Serialize result) ⇒
    IO configuration →
    (configuration → IO ()) →
    (configuration → IO (Maybe (Progress result))) →
    (configuration → ProcessesControllerMonad result ()) →
    (configuration → Visitor result) →
    IO (Maybe (configuration,RunOutcome result))
runVisitor getConfiguration initializeGlobalState getStartingProgress constructManager constructVisitor =
    genericRunVisitor
        forkVisitorWorkerThread
        getConfiguration
        initializeGlobalState
        getStartingProgress
        constructManager
        constructVisitor
-- }}}

runVisitorIO :: -- {{{
    (Serialize configuration, Monoid result, Serialize result) ⇒
    IO configuration →
    (configuration → IO ()) →
    (configuration → IO (Maybe (Progress result))) →
    (configuration → ProcessesControllerMonad result ()) →
    (configuration → VisitorIO result) →
    IO (Maybe (configuration,RunOutcome result))
runVisitorIO getConfiguration initializeGlobalState getStartingProgress constructManager constructVisitor =
    genericRunVisitor
        forkVisitorIOWorkerThread
        getConfiguration
        initializeGlobalState
        getStartingProgress
        constructManager
        constructVisitor
-- }}}

runVisitorT :: -- {{{
    (Serialize configuration, Monoid result, Serialize result, Functor m, MonadIO m) ⇒
    (∀ α. m α → IO α) →
    IO configuration →
    (configuration → IO ()) →
    (configuration → IO (Maybe (Progress result))) →
    (configuration → ProcessesControllerMonad result ()) →
    (configuration → VisitorT m result) →
    IO (Maybe (configuration,RunOutcome result))
runVisitorT runInBase getConfiguration initializeGlobalState getStartingProgress constructManager constructVisitor =
    genericRunVisitor
        (forkVisitorTWorkerThread runInBase)
        getConfiguration
        initializeGlobalState
        getStartingProgress
        constructManager
        constructVisitor
-- }}}

runWorkerWithVisitor :: -- {{{
    (Monoid result, Serialize result) ⇒
    Visitor result →
    Handle →
    Handle →
    IO ()
runWorkerWithVisitor = genericRunWorker . flip forkVisitorWorkerThread
-- }}}

runWorkerWithVisitorIO :: -- {{{
    (Monoid result, Serialize result) ⇒
    VisitorIO result →
    Handle →
    Handle →
    IO ()
runWorkerWithVisitorIO = genericRunWorker . flip forkVisitorIOWorkerThread
-- }}}

runWorkerWithVisitorT :: -- {{{
    (Monoid result, Serialize result, MonadIO m) ⇒
    (∀ α. m α → IO α) →
    VisitorT m result →
    Handle →
    Handle →
    IO ()
runWorkerWithVisitorT runInIO = genericRunWorker . flip (forkVisitorTWorkerThread runInIO)
-- }}}

-- }}}

-- Internal Functions {{{

fromJustOrBust message = fromMaybe (error message)

receive :: Serialize α ⇒ Handle → IO α -- {{{
receive handle = hGetLine handle >>= fmap (either error id . decode) . hGet handle . read
-- }}}

send :: Serialize α ⇒ Handle → α → IO () -- {{{
send handle value = do
    let encoded_value = encode value
    hPrint handle . BS.length $ encoded_value
    hPut handle encoded_value
    hFlush handle
-- }}}

genericRunVisitor :: -- {{{
    (Serialize configuration, Monoid result, Serialize result) ⇒
    (
        (WorkerTerminationReason result → IO ()) →
        visitor result →
        Workload →
        IO (WorkerEnvironment result)
    ) →
    IO configuration →
    (configuration → IO ()) →
    (configuration → IO (Maybe (Progress result))) →
    (configuration → ProcessesControllerMonad result ()) →
    (configuration → visitor result) →
    IO (Maybe (configuration,RunOutcome result))
genericRunVisitor forkVisitorWorkerThread getConfiguration initializeGlobalState getStartingProgress constructManager constructVisitor =
    getArgs >>= \args →
    if args == sentinel
        then do
            configuration ← receive stdin
            initializeGlobalState configuration
            genericRunWorker (flip forkVisitorWorkerThread . constructVisitor $ configuration) stdin stdout
            return Nothing
        else do
            configuration ← getConfiguration
            initializeGlobalState configuration
            program_filepath ← getProgFilepath
            maybe_starting_progress ← getStartingProgress configuration
            termination_result ←
                runSupervisor
                    program_filepath
                    sentinel
                    (flip send configuration)
                    maybe_starting_progress
                    (constructManager configuration)
            return $ Just (configuration,termination_result)
  where
    sentinel = ["visitor-worker-bee"]
-- }}}

genericRunWorker :: -- {{{
    (Monoid result, Serialize result) ⇒
    (
        (WorkerTerminationReason result → IO ()) →
        Workload →
        IO (WorkerEnvironment result)
    ) →
    Handle →
    Handle →
    IO ()
genericRunWorker spawnWorker read_handle write_handle = debugM "called genericRunWorker" >>
    Process.runWorker
        (receive read_handle)
        (send write_handle)
        spawnWorker
-- }}}

-- }}}
