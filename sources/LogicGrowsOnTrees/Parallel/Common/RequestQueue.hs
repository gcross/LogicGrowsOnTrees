{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DoRec #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

{-| To understand the purpose of this module, it helps to know that during the
    global exploration process there will be two main loops running in the
    supervisor. The first loop runs inside the 'SupervisorMonad' and is usually
    taken over by the adapter, which handles the communication between the
    supervisors and the workers. The second loop (usually referred to as the
    /controller/) is intended for the user of the adapter to be able to submit
    requests such as a global progress update to the supervisor, or possibly
    adapter-specific requests (such as changing the number of workers).

    With this in mind, the purpose of this module is to create infrastructure
    for the second loop (the controller) to submit requests to the first loop.
    It provides this functionality through a class so that specific adapters
    can extend this to provide requests specific to that adapter (such as
    changing the number of workers).
 -}
module LogicGrowsOnTrees.Parallel.Common.RequestQueue
    (
    -- * Type-classes
      RequestQueueMonad(..)
    -- * Types
    , Request
    , RequestQueue(..)
    , RequestQueueReader
    -- * Functions
    -- ** Synchronized requests
    , getCurrentProgress
    , getNumberOfWorkers
    , requestProgressUpdate
    , syncAsync
    -- ** Request queue management
    , addProgressReceiver
    , enqueueRequest
    , newRequestQueue
    , tryDequeueRequest
    -- ** Request processing
    , processAllRequests
    , receiveProgress
    , requestQueueProgram
    -- ** Controller threads
    , forkControllerThread
    , killControllerThreads
    -- ** Miscellaneous
    , getQuantityAsync
    ) where

import Prelude hiding (catch)

import Control.Applicative (liftA2)
import Control.Arrow ((&&&))
import Control.Concurrent (ThreadId,forkIO,killThread)
import Control.Concurrent.MVar (newEmptyMVar,putMVar,takeMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan,newTChanIO,readTChan,tryReadTChan,writeTChan)
import Control.Exception (BlockedIndefinitelyOnMVar(..),catch,finally)
import Control.Monad.CatchIO (MonadCatchIO)
import Control.Monad ((>=>),join,liftM,liftM3)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Reader (ReaderT(..),ask)

import Data.Composition ((.*))
import Data.Functor ((<$>))
import Data.IORef (IORef,atomicModifyIORef,readIORef,newIORef)
import Data.List (delete)

import qualified LogicGrowsOnTrees.Parallel.Common.Supervisor as Supervisor
import LogicGrowsOnTrees.Parallel.Common.Supervisor (SupervisorFullConstraint,SupervisorMonad,SupervisorProgram(..))
import LogicGrowsOnTrees.Parallel.ExplorationMode

--------------------------------------------------------------------------------
--------------------------------- Type-classes ---------------------------------
--------------------------------------------------------------------------------

{-| This class provides a set of supervisor requests common to all adapters. -}
class (HasExplorationMode m, MonadCatchIO m) ⇒ RequestQueueMonad m where
    {-| Abort the supervisor. -}
    abort :: m ()
    {-| Fork a new thread running in this monad. -}
    fork :: m () → m ThreadId
    {-| Request the current progress, and invoke the given callback with the result;  see 'getCurrentProgress' for the synchronous version. -}
    getCurrentProgressAsync :: (ProgressFor (ExplorationModeFor m) → IO ()) → m ()
    {-| Request the number of workers, and invoke the given callback with the result;  see 'getNumberOfWorkers' for the synchronous version. -}
    getNumberOfWorkersAsync :: (Int → IO ()) → m ()
    {-| Requests that a global progress update be performed, and invoke the given callback with the result;  see 'requestProgressUpdate' for the synchronous version. -}
    requestProgressUpdateAsync :: (ProgressFor (ExplorationModeFor m) → IO ()) → m ()
    {-| Sets the size of the workload buffer. -}
    setWorkloadBufferSize :: Int → m ()

--------------------------------------------------------------------------------
------------------------------------- Types ------------------------------------
--------------------------------------------------------------------------------

{-| The type of a supervisor request. -}
type Request exploration_mode worker_id m = SupervisorMonad exploration_mode worker_id m ()
{-| A basic supervisor request queue. -}
data RequestQueue exploration_mode worker_id m = RequestQueue
    {   {-| the queue of requests to the supervisor -}
        requests :: !(TChan (Request exploration_mode worker_id m))
        {-| a list of callbacks to invoke when a global progress update has completed -}
    ,   receivers :: !(IORef [ProgressFor exploration_mode → IO ()])
        {-| a list of the controller threads -}
    ,   controllerThreads  :: !(IORef [ThreadId])
    }
{-| A basic supervisor request queue monad, which has an implicit 'RequestQueue'
    object that it uses to communicate with the supervisor loop.
 -}
type RequestQueueReader exploration_mode worker_id m = ReaderT (RequestQueue exploration_mode worker_id m) IO

instance HasExplorationMode (RequestQueueReader exploration_mode worker_id m) where
    type ExplorationModeFor (RequestQueueReader exploration_mode worker_id m) = exploration_mode

instance (SupervisorFullConstraint worker_id m, MonadCatchIO m) ⇒ RequestQueueMonad (RequestQueueReader exploration_mode worker_id m) where
    abort = ask >>= enqueueRequest Supervisor.abortSupervisor
    fork m = ask >>= flip forkControllerThread m
    getCurrentProgressAsync = (ask >>=) . getQuantityAsync Supervisor.getCurrentProgress
    getNumberOfWorkersAsync = (ask >>=) . getQuantityAsync Supervisor.getNumberOfWorkers
    requestProgressUpdateAsync receiveUpdatedProgress =
        ask
        >>=
        liftIO
        .
        liftA2 (>>)
            (addProgressReceiver receiveUpdatedProgress)
            (enqueueRequest Supervisor.performGlobalProgressUpdate)
    setWorkloadBufferSize size = ask >>= enqueueRequest (Supervisor.setWorkloadBufferSize size)

--------------------------------------------------------------------------------
---------------------------------- Functions -----------------------------------
--------------------------------------------------------------------------------

------------------------------ Synchronized requests ------------------------------

{-| Like 'getCurrentProgressAsync', but blocks until the result is ready. -}
getCurrentProgress :: RequestQueueMonad m ⇒ m (ProgressFor (ExplorationModeFor m))
getCurrentProgress = syncAsync getCurrentProgressAsync

{-| Like 'getNumberOfWorkersAsync', but blocks until the result is ready. -}
getNumberOfWorkers :: RequestQueueMonad m ⇒ m Int
getNumberOfWorkers = syncAsync getNumberOfWorkersAsync

{-| Like 'requestProgressUpdateAsync', but blocks until the progress update has completed. -}
requestProgressUpdate :: RequestQueueMonad m ⇒ m (ProgressFor (ExplorationModeFor m))
requestProgressUpdate = syncAsync requestProgressUpdateAsync

{-| General utility function for converting an asynchronous request to a
    syncronous request;  it uses an 'MVar' to hold the result of the request and
    blocks until the 'MVar' has been filled.
 -}
syncAsync :: MonadIO m ⇒ ((α → IO ()) → m ()) → m α
syncAsync runCommandAsync = do
    result_mvar ← liftIO newEmptyMVar
    runCommandAsync (putMVar result_mvar)
    liftIO $
        takeMVar result_mvar
        `catch`
        (\BlockedIndefinitelyOnMVar → error $ "blocked forever while waiting for controller to respond to request")

---------------------------- Request queue management -----------------------------

{-| Adds a callback to the given 'RequestQueue' that will be invoked when the current global progress update has completed. -}
addProgressReceiver ::
    MonadIO m' ⇒
    (ProgressFor exploration_mode → IO ()) →
    RequestQueue exploration_mode worker_id m →
    m' ()
addProgressReceiver receiver =
    liftIO
    .
    flip atomicModifyIORef ((receiver:) &&& const ())
    .
    receivers

{-| Enqueues a supervisor request into the given queue. -}
enqueueRequest ::
    MonadIO m' ⇒
    Request exploration_mode worker_id m →
    RequestQueue exploration_mode worker_id m →
    m' ()
enqueueRequest = flip $
    (liftIO . atomically)
    .*
    (writeTChan . requests)

{-| Constructs a new 'RequestQueue'. -}
newRequestQueue ::
    MonadIO m' ⇒
    m' (RequestQueue exploration_mode worker_id m)
newRequestQueue = liftIO $ liftM3 RequestQueue newTChanIO (newIORef []) (newIORef [])

{-| Attempt to pop a request from the 'RequestQueue'. -}
tryDequeueRequest ::
    MonadIO m' ⇒
    RequestQueue exploration_mode worker_id m →
    m' (Maybe (Request exploration_mode worker_id m))
tryDequeueRequest =
    liftIO
    .
    atomically
    .
    tryReadTChan
    .
    requests

------------------------------- Request processing --------------------------------

{-| Processes all of the requests in the given 'RequestQueue', and returns when
    the queue has been emptied.
 -}
processAllRequests ::
    MonadIO m ⇒
    RequestQueue exploration_mode worker_id m →
    SupervisorMonad exploration_mode worker_id m ()
processAllRequests (RequestQueue requests _ _) = go
  where
    go =
        (liftIO . atomically . tryReadTChan) requests
        >>=
        maybe (return ()) (>> go)

{-| Invokes all of the callbacks with the given progress and then clears the list of callbacks. -}
receiveProgress ::
    MonadIO m' ⇒
    RequestQueue exploration_mode worker_id m →
    ProgressFor exploration_mode →
    m' ()
receiveProgress queue progress =
    liftIO
    .
    join
    .
    liftM (sequence_ . map ($ progress))
    .
    flip atomicModifyIORef (const [] &&& id)
    .
    receivers
    $
    queue

{-| Creates a supervisor program that loops forever processing requests from the given queue. -}
requestQueueProgram ::
    MonadIO m ⇒
    SupervisorMonad exploration_mode worker_id m () {-^ initialization code to run before the loop is started -} →
    RequestQueue exploration_mode worker_id m {-^ the request queue -} →
    SupervisorProgram exploration_mode worker_id m
requestQueueProgram initialize =
    flip (BlockingProgram initialize) id
    .
    liftIO
    .
    atomically
    .
    readTChan
    .
    requests

------------------------------ Controller threads ------------------------------

{-| Adds a controller thread id to the request queue. -}
forkControllerThread ::
    MonadIO m' ⇒
    RequestQueue exploration_mode worker_id m {-^ the request queue -} →
    RequestQueueReader exploration_mode worker_id m () {-^ the controller thread -} →
    m' ThreadId
forkControllerThread request_queue controller = liftIO $ do
    start_signal ← newEmptyMVar
    rec thread_id ←
            forkIO
            $
            (takeMVar start_signal >> runReaderT controller request_queue)
            `finally`
            (atomicModifyIORef (controllerThreads request_queue) (delete thread_id &&& const ()))
    atomicModifyIORef (controllerThreads request_queue) ((thread_id:) &&& const ())
    {- NOTE:  The following signal is needed because we don't want the new
              thread to start until after its thread id has been added to the
              list, as otherwise it could result in an orphan thread that won't
              get garbage collected until the supervisor finishes due to a
              pecularity of the GHC runtime where it doesn't garbage collect a
              thread as long as a ThreadId referring to it exists.
     -}
    putMVar start_signal ()
    return thread_id

{-| Kill all the remaining worker threads. -}
killControllerThreads ::
    MonadIO m' ⇒
    RequestQueue exploration_mode worker_id m {-^ the request queue -} →
    m' ()
killControllerThreads = liftIO . readIORef . controllerThreads >=> liftIO . mapM_ killThread

---------------------------------- Miscellaneous ----------------------------------

{-| Submits a 'Request' to the supervisor and invoks the given callback with the
    result when it is available.  (This function is used by
    'getCurrentProgressAsync' and 'getNumberOfWorkersAsync'.)
 -}
getQuantityAsync ::
    ( MonadIO m'
    , SupervisorFullConstraint worker_id m
    ) ⇒
    SupervisorMonad exploration_mode worker_id m α →
    (α → IO ()) →
    RequestQueue exploration_mode worker_id m →
    m' ()
getQuantityAsync getQuantity receiveQuantity =
    enqueueRequest $ getQuantity >>= liftIO . receiveQuantity
