-- Language extensions {{{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Control.Monad.Trans.Visitor.Parallel.Processes
    ( getProgFilepath
    , runSupervisor
    , runWorkerWithVisitor
    , runWorkerWithVisitorIO
    , runWorkerWithVisitorT
    ) where

-- Imports {{{
import Prelude hiding (catch)

import Control.Applicative (Applicative)
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

import Options.Applicative (InfoMod,execParser,info)

import System.Environment (getProgName)
import System.Environment.FindBin (getProgPath)
import System.FilePath ((</>))
import System.IO (Handle,hFlush,hGetLine,hPrint)
import System.IO.Error (isEOFError)
import qualified System.Log.Logger as Logger
import System.Log.Logger (Priority(DEBUG,INFO,ERROR))
import System.Log.Logger.TH
import System.Process (CreateProcess(..),CmdSpec(RawCommand),StdStream(..),ProcessHandle,createProcess,interruptProcessGroupOf)

import Control.Monad.Trans.Visitor (Visitor,VisitorIO,VisitorT)
import Control.Monad.Trans.Visitor.Checkpoint
import Control.Monad.Trans.Visitor.Parallel.Workgroup
import Control.Monad.Trans.Visitor.Supervisor.Driver (Driver(Driver),TerminationReason)
import Control.Monad.Trans.Visitor.Supervisor.RequestQueue
import Control.Monad.Trans.Visitor.Worker as Worker
import qualified Control.Monad.Trans.Visitor.Worker.Process as Process
import Control.Monad.Trans.Visitor.Worker.Process
import Control.Monad.Trans.Visitor.Workload
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

-- Exposed Functions {{{

getProgFilepath :: IO String -- {{{
getProgFilepath = liftM2 (</>) getProgPath getProgName
-- }}}

runSupervisor :: -- {{{
    (Monoid result, Serialize result) ⇒
    String →
    [String] →
    Maybe (VisitorProgress result) →
    ProcessesControllerMonad result () →
    IO (TerminationReason result)
runSupervisor worker_filepath worker_arguments maybe_starting_progress (C controller) = do
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
                    _ ← liftIO . forkIO $
                        catchJust
                            (\e → if isEOFError e then Just () else Nothing)
                            (forever $ hGetLine error_handle >>= \line → debugM $ "[" ++ show worker_id ++ "] " ++ line)
                            (const $ return ())
                         `catch`
                            (\(e::SomeException) → errorM $ "Error reading stderr for worker " ++ show worker_id ++ ": " ++ show e)
                    _ ← liftIO . forkIO $ (
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
                sendProgressUpdateRequest = sendMessageToWorker RequestProgressUpdate
                sendWorkloadStealRequest = sendMessageToWorker RequestWorkloadSteal
                sendWorkloadToWorker worker_id workload = sendMessageToWorker (Workload workload) worker_id
            in WorkgroupCallbacks{..}
        ) -- }}}
        maybe_starting_progress
        controller
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

genericRunWorker :: -- {{{
    (Monoid result, Serialize result) ⇒
    (
        (VisitorWorkerTerminationReason result → IO ()) →
        VisitorWorkload →
        IO (VisitorWorkerEnvironment result)
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
