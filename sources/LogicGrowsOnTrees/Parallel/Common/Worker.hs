{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

{-| The 'Worker' module contains the workhorse code of the parallelization
    infrastructure in the form of the 'forkWorkerThread' function, which
    explores a tree step by step while continuously polling for requests; for
    more details see 'forkWorkerThread'.
 -}
module LogicGrowsOnTrees.Parallel.Common.Worker
    (
    -- * Types
    -- ** Worker interaction
      ProgressUpdate(..)
    , ProgressUpdateFor
    , StolenWorkload(..)
    , StolenWorkloadFor
    , WorkerRequestQueue
    , WorkerRequestQueueFor
    , WorkerEnvironment(..)
    , WorkerEnvironmentFor
    , WorkerTerminationReason(..)
    , WorkerTerminationReasonFor
    , WorkerPushActionFor
    -- * Functions
    , forkWorkerThread
    , sendAbortRequest
    , sendProgressUpdateRequest
    , sendWorkloadStealRequest
    , exploreTreeGeneric
    ) where

import Prelude hiding (catch)

import Control.Arrow ((&&&))
import Control.Concurrent (forkIO,ThreadId,yield)
import Control.Concurrent.MVar (newEmptyMVar,putMVar,takeMVar)
import Control.Exception (AsyncException(ThreadKilled,UserInterrupt),catch,fromException)
import Control.Monad (liftM)
import Control.Monad.IO.Class

import Data.Composition
import Data.Derive.Serialize
import Data.DeriveTH
import Data.Functor ((<$>))
import Data.IORef (atomicModifyIORef,IORef,newIORef,readIORef)
import qualified Data.IVar as IVar
import Data.IVar (IVar)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>),Monoid(..))
import Data.Sequence ((|>),(><),viewl,ViewL(..))
import Data.Serialize
import qualified Data.Sequence as Seq
import Data.Void (Void,absurd)

import qualified System.Log.Logger as Logger
import System.Log.Logger (Priority(DEBUG))
import System.Log.Logger.TH

import LogicGrowsOnTrees hiding (exploreTree,exploreTreeT,exploreTreeUntilFirst,exploreTreeTUntilFirst)
import LogicGrowsOnTrees.Checkpoint
import LogicGrowsOnTrees.Parallel.ExplorationMode
import LogicGrowsOnTrees.Parallel.Purity
import LogicGrowsOnTrees.Path
import LogicGrowsOnTrees.Workload


--------------------------------------------------------------------------------
------------------------------------ Types -------------------------------------
--------------------------------------------------------------------------------

------------------------------ Worker responses --------------------------------

{-| A progress update sent to the supervisor;  it has a component which contains
    information about how much of the tree has been explored and what results
    have been found so far, as well as the remaining 'Workload' to be completed
    by this worker.
 -}
data ProgressUpdate progress = ProgressUpdate
    {   progressUpdateProgress :: progress
    ,   progressUpdateRemainingWorkload :: Workload
    } deriving (Eq,Show)
$( derive makeSerialize ''ProgressUpdate )

{-| A convenient type alias for the type of 'ProgressUpdate' associated with the
    given exploration mode.
 -}
type ProgressUpdateFor exploration_mode = ProgressUpdate (ProgressFor exploration_mode)

{-| A stolen workload sent to the supervisor;  in addition to a component with
    the stolen 'Workload' itself, it also has a 'ProgressUpdate' component,
    which is required in order to maintain the invariant that all of the
    'Workload's that the supervisor has on file (both assigned to workers and
    unassigned) plus the current progress equals the full tree.
 -}
data StolenWorkload progress = StolenWorkload
    {   stolenWorkloadProgressUpdate :: ProgressUpdate progress
    ,   stolenWorkload :: Workload
    } deriving (Eq,Show)
$( derive makeSerialize ''StolenWorkload )

{-| A convenient type alias for the type of 'StolenWorkload' associated with the
    the given exploration mode.
 -}
type StolenWorkloadFor exploration_mode = StolenWorkload (ProgressFor exploration_mode)

------------------------------- Worker requests --------------------------------

{-| A worker request. -}
data WorkerRequest progress =
    {-| Request that the worker abort. -}
    AbortRequested
    {-| Request that the worker respond with a progress update. -}
  | ProgressUpdateRequested (ProgressUpdate progress → IO ())
    {-| Request that the worker respond with a stolen workload (if possible). -}
  | WorkloadStealRequested (Maybe (StolenWorkload progress) → IO ())

{-| A queue of worker requests.

    NOTE:  Although the type is a list, and requests are added by prepending
    them to the list, it still acts as a queue because the worker will reverse
    the list before processing the requests.
-}
type WorkerRequestQueue progress = IORef [WorkerRequest progress]
{-| A convenient type alias for the type of 'WorkerRequestQueue' associated with
    the given exploration mode. -}
type WorkerRequestQueueFor exploration_mode = WorkerRequestQueue (ProgressFor exploration_mode)

--------------------------------- Worker types ---------------------------------

{-| The environment of a running worker. -}
data WorkerEnvironment progress = WorkerEnvironment
    {   workerInitialPath :: Path {-^ the initial path of the worker's workload -}
    ,   workerThreadId :: ThreadId {-^ the thread id of the worker thread -}
    ,   workerPendingRequests :: WorkerRequestQueue progress {-^ the request queue for the worker -}
    ,   workerTerminationFlag :: IVar () {-^ an IVar that is filled when the worker terminates -}
    }

{-| A convenient type alias for the type of 'WorkerEnvironment' associated with
    the given exploration mode. -}
type WorkerEnvironmentFor exploration_mode = WorkerEnvironment (ProgressFor exploration_mode)

{-| The reason why a worker terminated. -}
data WorkerTerminationReason worker_final_progress =
    {-| worker completed normally without error;  included is the final result -}
    WorkerFinished worker_final_progress
    {-| worker failed;  included is the message of the failure (this would have
        been a value of type 'SomeException' if it were not for the fact that
        this value will often have to be sent over communication channels and
        exceptions cannot be serialized (as they have unknown type), meaning
        that it usually has to be turned into a 'String' via 'show' anyway)
     -}
  | WorkerFailed String
    {-| worker was aborted by either an external request or the 'ThreadKilled' or 'UserInterrupt' exceptions -}
  | WorkerAborted
  deriving (Eq,Show)

{-| The 'Functor' instance lets you manipulate the final progress value when
    the termination reason is 'WorkerFinished'.
 -}
instance Functor WorkerTerminationReason where
    fmap f (WorkerFinished x) = WorkerFinished (f x)
    fmap _ (WorkerFailed x) = WorkerFailed x
    fmap _ WorkerAborted = WorkerAborted

{-| A convenient type alias for the type of 'WorkerTerminationReason' associated
    with the given exploration mode.
 -}
type WorkerTerminationReasonFor exploration_mode = WorkerTerminationReason (WorkerFinalProgressFor exploration_mode)

{-| The action that a worker can take to push a result to the supervisor;  this
    type is effectively null (with the exact value 'absurd') for all modes
    except 'FoundModeUsingPush'.
 -}
type family WorkerPushActionFor exploration_mode :: *
-- NOTE:  Setting the below instances equal to Void → () is a hack around the
--        fact that using types with constructors result in a weird compiler bug.
type instance WorkerPushActionFor (AllMode result) = Void → ()
type instance WorkerPushActionFor (FirstMode result) = Void → ()
type instance WorkerPushActionFor (FoundModeUsingPull result) = Void → ()
type instance WorkerPushActionFor (FoundModeUsingPush result) = ProgressUpdate (Progress result) → IO ()

------------------------------- Tree properties --------------------------------

{-| Functions for working with a tree of a particular purity. -}
data TreeFunctionsForPurity (m :: * → *) (n :: * → *) (α :: *) =
    MonadIO n ⇒ TreeFunctionsForPurity
    {   walk :: Path → TreeT m α → n (TreeT m α)
    ,   step :: ExplorationTState m α → n (Maybe α,Maybe (ExplorationTState m α))
    ,   run ::  (∀ β. n β → IO β)
    }

--------------------------------------------------------------------------------
----------------------------------- Loggers ------------------------------------
--------------------------------------------------------------------------------

deriveLoggers "Logger" [DEBUG]

--------------------------------------------------------------------------------
----------------------------- Main worker function -----------------------------
--------------------------------------------------------------------------------

{-| The 'forkWorkerThread' function is the workhorse of the parallization
    infrastructure; it explores a tree in a separate thread while polling for
    requests. Specifically, the worker alternates between stepping through the
    tree and checking to see if there are any new requests in the queue.
    
    The worker is optimized around the assumption that requests are rare. For
    this reason, it uses an 'IORef' for the queue to minimize the cost of
    peeking at it rather than an 'MVar' or other thread synchronization
    variable; the trade-off is that if a request is added to the queue by a
    different processor then it might not be noticed immediately until the
    caches get synchronized; since requests are rare this not a significant cost
    to pay. Likewise, the request queue uses the list type rather than something
    like "Data.Sequence" for simplicity; the vast majority of the time the
    worker will encounter an empty list, and on the rare occasion when the list
    is non-empty it will be short enough that 'reverse'ing it will not pose a
    significant cost.

    At any given point in the exploration, there is an initial path which
    locates the subtree that was given as the original workload, a cursor which
    indicates the subtree /within/ this subtree that makes up the /current/
    workload, and the context which indicates the current location in the
    subtree that is being explored. All workers start with an empty cursor; when
    a workload is stolen, decisions made early on in the the context are frozen
    and moved into the cursor because if they were not then when the worker
    backtracked it would explore a workload that it just gave away, resulting in
    some results being observed twice.

    The worker terminates either if it finishes exploring all the nodes in its
    (current) workload, if an error occurs, or if it is aborted either via.
    the 'ThreadKilled' and 'UserInterrupt' exceptions or by an abort request
    placed in the request queue.
 -}
forkWorkerThread ::
    ExplorationMode exploration_mode {-^ the mode in to explore the tree -} →
    Purity m n {-^ the purity of the tree -} →
    (WorkerTerminationReasonFor exploration_mode → IO ()) {-^ the action to run when the worker has terminated -} →
    TreeT m (ResultFor exploration_mode) {-^ the tree -} →
    Workload {-^ the workload for the worker -} →
    WorkerPushActionFor exploration_mode
        {-^ the action to push a result to the supervisor;  this should be equal
            to 'absurd' except when the exploration mode is 'FoundModeUsingPush'.
         -} →
    IO (WorkerEnvironmentFor exploration_mode) {-^ the environment for the worker -}
forkWorkerThread
    exploration_mode
    purity
    finishedCallback
    tree
    (Workload initial_path initial_checkpoint)
    push
  = do
    -- Note:  the following line of code needs to be this way --- that is, using
    --        do notation to extract the value of TreeFunctionsForPurity --- or else
    --        GHC's head will explode!
    TreeFunctionsForPurity{..} ← case getTreeFunctionsForPurity purity of x → return x
    pending_requests_ref ← newIORef []

    ------------------------------- LOOP PHASES --------------------------------
    let
    -- LOOP PHASE 1:  Check if there are any pending requests;  if so, proceed
    --                to phase 2;  otherwise (the most common case), skip to
    --                phase 3.
        loop1 (!result) cursor exploration_state =
            liftIO (readIORef pending_requests_ref) >>= \pending_requests →
            case pending_requests of
                [] → loop3 result cursor exploration_state
                _ → debugM "Worker thread's request queue is non-empty."
                    >> (liftM reverse . liftIO $ atomicModifyIORef pending_requests_ref (const [] &&& id))
                    >>= loop2 result cursor exploration_state

    -- LOOP PHASE 2:  Process all pending requests.
        loop2 result cursor exploration_state@(ExplorationTState context checkpoint tree) requests =
            case requests of
                [] → liftIO yield >> loop3 result cursor exploration_state
                AbortRequested:_ → do
                    debugM "Worker theread received abort request."
                    return WorkerAborted
                ProgressUpdateRequested submitProgress:rest_requests → do
                    debugM "Worker thread received progress update request."
                    liftIO . submitProgress $ computeProgressUpdate exploration_mode result initial_path cursor context checkpoint
                    loop2 initial_intermediate_value cursor exploration_state rest_requests
                WorkloadStealRequested submitMaybeWorkload:rest_requests → do
                    debugM "Worker thread received workload steal."
                    case tryStealWorkload initial_path cursor context of
                        Nothing → do
                            liftIO $ submitMaybeWorkload Nothing
                            loop2 result cursor exploration_state rest_requests
                        Just (new_cursor,new_context,workload) → do
                            liftIO . submitMaybeWorkload . Just $
                                StolenWorkload
                                    (computeProgressUpdate exploration_mode result initial_path new_cursor new_context checkpoint)
                                    workload
                            loop2 initial_intermediate_value new_cursor (ExplorationTState new_context checkpoint tree) rest_requests

    -- LOOP PHASE 3:  Take a step through the tree, and then (if not finished)
    --                return to phase 1.
        loop3 result cursor exploration_state
          = do
            (maybe_solution,maybe_new_exploration_state) ← step exploration_state 
            case maybe_new_exploration_state of
                Nothing → -- We have completed exploring the tree.
                    let explored_checkpoint =
                            checkpointFromInitialPath initial_path
                            .
                            checkpointFromCursor cursor
                            $
                            Explored
                    in return . WorkerFinished $
                        -- NOTE:  Do *not* refactor the code below; if you do so
                        --        then it will confuse the type-checker.
                        case exploration_mode of
                            AllMode →
                                Progress
                                    explored_checkpoint
                                    (maybe result (result <>) maybe_solution)
                            FirstMode →
                                Progress
                                    explored_checkpoint
                                    maybe_solution
                            FoundModeUsingPull _ →
                                Progress
                                    explored_checkpoint
                                    (maybe result (result <>) maybe_solution)
                            FoundModeUsingPush _ →
                                Progress
                                    explored_checkpoint
                                    (fromMaybe mempty maybe_solution)
                Just new_exploration_state@(ExplorationTState context checkpoint _) →
                    let new_checkpoint = checkpointFromSetting initial_path cursor context checkpoint
                    in case maybe_solution of
                        Nothing → loop1 result cursor new_exploration_state
                        Just (!solution) →
                            case exploration_mode of
                                AllMode → loop1 (result <> solution) cursor new_exploration_state
                                FirstMode → return . WorkerFinished $ Progress new_checkpoint (Just solution)
                                FoundModeUsingPull f →
                                    let new_result = result <> solution
                                    in if f new_result
                                        then return . WorkerFinished $ Progress new_checkpoint new_result
                                        else loop1 new_result cursor new_exploration_state
                                FoundModeUsingPush _ → do
                                    liftIO . push $
                                        ProgressUpdate
                                            (Progress new_checkpoint solution)
                                            (workloadFromSetting initial_path cursor context checkpoint)
                                    loop1 () cursor new_exploration_state

    ----------------------------- LAUNCH THE WORKER ----------------------------
    finished_flag ← IVar.new
    thread_id ← forkIO $ do
        termination_reason ←
            debugM "Worker thread has been forked." >>
            (run $
                walk initial_path tree
                >>=
                loop1 initial_intermediate_value Seq.empty
                .
                initialExplorationState initial_checkpoint
            )
            `catch`
            (\e → case fromException e of
                Just ThreadKilled → return WorkerAborted
                Just UserInterrupt → return WorkerAborted
                _ → return $ WorkerFailed (show e)
            )
        debugM $ "Worker thread has terminated with reason " ++
            case termination_reason of
                WorkerFinished _ → "finished."
                WorkerFailed message → "failed: " ++ show message
                WorkerAborted → "aborted."
        IVar.write finished_flag ()
        finishedCallback termination_reason
    return $
        WorkerEnvironment
            initial_path
            thread_id
            pending_requests_ref
            finished_flag
  where
    initial_intermediate_value = initialWorkerIntermediateValue exploration_mode
{-# INLINE forkWorkerThread #-}

--------------------------------------------------------------------------------
------------------------------ Request functions ------------------------------
--------------------------------------------------------------------------------

{-| Sends a request to abort. -}
sendAbortRequest :: WorkerRequestQueue progress → IO ()
sendAbortRequest = flip sendRequest AbortRequested

{-| Sends a request for a progress update along with a response action to
    perform when the progress update is available.
 -}
sendProgressUpdateRequest ::
    WorkerRequestQueue progress {-^ the request queue -} →
    (ProgressUpdate progress → IO ()) {-^ the action to perform when the update is available -} →
    IO ()
sendProgressUpdateRequest queue = sendRequest queue . ProgressUpdateRequested

{-| Sends a request to steal a workload along with a response action to
    perform when the progress update is available.
 -}
sendWorkloadStealRequest ::
    WorkerRequestQueue progress {-^ the request queue -} →
    (Maybe (StolenWorkload progress) → IO ()) {-^ the action to perform when the update is available -} →
    IO ()
sendWorkloadStealRequest queue = sendRequest queue . WorkloadStealRequested

{-| Sends a request to a worker. -}
sendRequest :: WorkerRequestQueue progress → WorkerRequest progress → IO ()
sendRequest queue request = atomicModifyIORef queue ((request:) &&& const ())

--------------------------------------------------------------------------------
------------------------------ Utility functions -------------------------------
--------------------------------------------------------------------------------

{-| Explores a tree with the specified purity using the given mode by forking a
    worker thread and waiting for it to finish; it exists to facilitate testing
    and benchmarking and is not a function that you are likely to ever have a
    need for yourself.
 -}
exploreTreeGeneric ::
    ( WorkerPushActionFor exploration_mode ~ (Void → ())
    , ResultFor exploration_mode ~ α
    ) ⇒
    ExplorationMode exploration_mode →
    Purity m n →
    TreeT m α →
    IO (WorkerTerminationReason (FinalResultFor exploration_mode))
exploreTreeGeneric exploration_mode purity tree = do
    final_progress_mvar ← newEmptyMVar
    _ ← forkWorkerThread
            exploration_mode
            purity
            (putMVar final_progress_mvar)
            tree
            entire_workload
            absurd
    final_progress ← takeMVar final_progress_mvar
    return . flip fmap final_progress $ \progress →
        case exploration_mode of
            AllMode → progressResult progress
            FirstMode → Progress (progressCheckpoint progress) <$> progressResult progress
            FoundModeUsingPull f →
                if f (progressResult progress)
                    then Right progress
                    else Left (progressResult progress)
            _ → error "should never reach here due to incompatible types"

--------------------------------------------------------------------------------
------------------------------ Internal functions ------------------------------
--------------------------------------------------------------------------------

checkpointFromSetting ::
    Path →
    CheckpointCursor →
    Context m α →
    Checkpoint →
    Checkpoint
checkpointFromSetting initial_path cursor context =
     checkpointFromInitialPath initial_path
     .
     checkpointFromCursor cursor
     .
     checkpointFromContext context

computeProgressUpdate ::
    ResultFor exploration_mode ~ α ⇒
    ExplorationMode exploration_mode →
    WorkerIntermediateValueFor exploration_mode →
    Path →
    CheckpointCursor →
    Context m α →
    Checkpoint →
    ProgressUpdate (ProgressFor exploration_mode)
computeProgressUpdate exploration_mode result initial_path cursor context checkpoint =
    ProgressUpdate
        (case exploration_mode of
            AllMode → Progress full_checkpoint result
            FirstMode → full_checkpoint
            FoundModeUsingPull _ → Progress full_checkpoint result
            FoundModeUsingPush _ → Progress full_checkpoint mempty
        )
        (workloadFromSetting initial_path cursor context checkpoint)
  where
    full_checkpoint = checkpointFromSetting initial_path cursor context checkpoint

getTreeFunctionsForPurity :: Purity m n → TreeFunctionsForPurity m n α
getTreeFunctionsForPurity Pure = TreeFunctionsForPurity{..}
  where
    walk = return .* sendTreeDownPath
    step = return . stepThroughTreeStartingFromCheckpoint
    run = id
getTreeFunctionsForPurity (ImpureAtopIO run) = TreeFunctionsForPurity{..}
  where
    walk = sendTreeTDownPath
    step = stepThroughTreeTStartingFromCheckpoint
{-# INLINE getTreeFunctionsForPurity #-}

tryStealWorkload ::
    Path →
    CheckpointCursor →
    Context m α →
    Maybe (CheckpointCursor,Context m α,Workload)
tryStealWorkload initial_path = go
  where
    go cursor context =
        case viewl context of
            EmptyL → Nothing
            CacheContextStep cache :< rest_context →
                go (cursor |> CachePointD cache) rest_context
            LeftBranchContextStep other_checkpoint _ :< rest_context →
                Just (cursor |> ChoicePointD LeftBranch Unexplored
                     ,rest_context
                     ,Workload
                        ((initial_path >< pathFromCursor cursor) |> ChoiceStep RightBranch)
                        other_checkpoint
                     )
            RightBranchContextStep :< rest_context →
                go (cursor |> ChoicePointD RightBranch Explored) rest_context

workloadFromSetting ::
    Path →
    CheckpointCursor →
    Context m α →
    Checkpoint →
    Workload
workloadFromSetting initial_path cursor context =
    Workload (initial_path >< pathFromCursor cursor)
    .
    checkpointFromContext context
