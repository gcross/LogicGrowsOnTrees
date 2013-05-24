-- Language extensions {{{
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- }}}

module Control.Visitor.Parallel.Common.Worker
    ( ProgressUpdate(..)
    , StolenWorkload(..)
    , VisitorKind(..)
    , WorkerRequestQueue
    , WorkerEnvironment(..)
    , WorkerTerminationReason(..)
    , forkVisitorWorkerThread
    , forkVisitorIOWorkerThread
    , forkVisitorTWorkerThread
    , genericForkVisitorTWorkerThread
    , runVisitor
    , runVisitorIO
    , runVisitorT
    , sendAbortRequest
    , sendProgressUpdateRequest
    , sendWorkloadStealRequest
    ) where

-- Imports {{{
import Prelude hiding (catch)

import Control.Arrow ((&&&))
import Control.Concurrent (forkIO,killThread,threadDelay,ThreadId,yield)
import Control.Concurrent.MVar (newEmptyMVar,putMVar,takeMVar)
import Control.Exception (AsyncException(ThreadKilled,UserInterrupt),catch,evaluate,fromException)
import Control.Monad (liftM)
import Control.Monad.IO.Class

import Data.Bool.Higher ((??))
import Data.Composition
import Data.Derive.Serialize
import Data.DeriveTH
import qualified Data.Foldable as Fold
import Data.Functor.Identity (Identity)
import Data.IORef (atomicModifyIORef,IORef,newIORef,readIORef,writeIORef)
import qualified Data.IVar as IVar
import Data.IVar (IVar)
import Data.Monoid (Monoid(..))
import Data.Maybe (isJust)
import Data.Sequence ((|>),(><),Seq,viewl,ViewL(..))
import Data.Serialize
import qualified Data.Sequence as Seq

import qualified System.Log.Logger as Logger
import System.Log.Logger (Priority(DEBUG,INFO))
import System.Log.Logger.TH

import Control.Visitor hiding (runVisitor,runVisitorT)
import Control.Visitor.Checkpoint
import Control.Visitor.Parallel.Common.VisitorMode
import Control.Visitor.Path
import Control.Visitor.Workload
-- }}}

-- Types {{{

data ProgressUpdate ip = ProgressUpdate -- {{{
    {   progressUpdateProgress :: ip
    ,   progressUpdateRemainingWorkload :: Workload
    } deriving (Eq,Show)
$( derive makeSerialize ''ProgressUpdate )
-- }}}

data StolenWorkload ip = StolenWorkload -- {{{
    {   stolenWorkloadProgressUpdate :: ProgressUpdate ip
    ,   stolenWorkload :: Workload
    } deriving (Eq,Show)
$( derive makeSerialize ''StolenWorkload )
-- }}}

data VisitorKind (m :: * → *) (n :: * → *) where -- {{{
    PureVisitor :: VisitorKind Identity IO
    IOVisitor :: VisitorKind IO IO
    ImpureVisitor :: MonadIO m ⇒ (∀ β. m β → IO β) → VisitorKind m m
-- }}}

data VisitorFunctions (m :: * → *) (n :: * → *) (α :: *) = -- {{{
    ∀ n. MonadIO n ⇒ VisitorFunctions
    {   walk :: Path → VisitorT m α → n (VisitorT m α)
    ,   step :: VisitorTState m α → n (Maybe α,Maybe (VisitorTState m α))
    ,   run ::  (∀ β. n β → IO β)
    }
-- }}}

data WorkerRequest ip = -- {{{
    AbortRequested
  | ProgressUpdateRequested (ProgressUpdate ip → IO ())
  | WorkloadStealRequested (Maybe (StolenWorkload ip) → IO ())
-- }}}

type WorkerRequestQueue ip = IORef [WorkerRequest ip]

data WorkerEnvironment ip = WorkerEnvironment -- {{{
    {   workerInitialPath :: Path
    ,   workerThreadId :: ThreadId
    ,   workerPendingRequests :: WorkerRequestQueue ip
    ,   workerTerminationFlag :: IVar ()
    }
-- }}}

data WorkerTerminationReason fp = -- {{{
    WorkerFinished fp
  | WorkerFailed String
  | WorkerAborted
  deriving (Show)
-- }}}

-- }}}

-- Logging Functions {{{
deriveLoggers "Logger" [DEBUG,INFO]
-- }}}

-- Functions {{{

computeProgressUpdate :: -- {{{
    VisitorMode α iv ip fv fp →
    iv →
    Path →
    CheckpointCursor →
    Context m α →
    Checkpoint →
    ProgressUpdate ip
computeProgressUpdate visitor_mode result initial_path cursor context checkpoint =
    ProgressUpdate
        (case visitor_mode of
            AllMode → Progress current_checkpoint result
            FirstMode → current_checkpoint
        )
        (Workload (initial_path >< pathFromCursor cursor)
         .
         checkpointFromContext context
         $
         checkpoint
        )
  where
    current_checkpoint =
        checkpointFromInitialPath initial_path
        .
        checkpointFromCursor cursor
        .
        checkpointFromContext context
        $
        checkpoint
-- }}}

extractAllFinishedResult :: WorkerTerminationReason (Progress α) → WorkerTerminationReason α -- {{{
extractAllFinishedResult (WorkerFinished Progress{..}) = WorkerFinished progressResult
extractAllFinishedResult (WorkerFailed message) = WorkerFailed message
extractAllFinishedResult WorkerAborted = WorkerAborted
-- }}}

forkVisitorWorkerThread :: -- {{{
    Monoid α ⇒
    (WorkerTerminationReason (Progress α) → IO ()) →
    Visitor α →
    Workload →
    IO (WorkerEnvironment (Progress α))
forkVisitorWorkerThread = genericForkVisitorTWorkerThread AllMode PureVisitor
-- }}}

forkVisitorIOWorkerThread :: -- {{{
    Monoid α ⇒
    (WorkerTerminationReason (Progress α) → IO ()) →
    VisitorIO α →
    Workload →
    IO (WorkerEnvironment (Progress α))
forkVisitorIOWorkerThread = genericForkVisitorTWorkerThread AllMode IOVisitor
-- }}}

forkVisitorTWorkerThread :: -- {{{
    (MonadIO m, Monoid α) ⇒
    (∀ β. m β → IO β) →
    (WorkerTerminationReason (Progress α) → IO ()) →
    VisitorT m α →
    Workload →
    IO (WorkerEnvironment (Progress α))
forkVisitorTWorkerThread =
    genericForkVisitorTWorkerThread AllMode
    .
    ImpureVisitor
-- }}}

getVisitorFunctions :: VisitorKind m n → VisitorFunctions m n α -- {{{
getVisitorFunctions PureVisitor = VisitorFunctions{..}
  where
    walk = return .* sendVisitorDownPath
    step = return . stepVisitorThroughCheckpoint
    run = id
getVisitorFunctions IOVisitor = VisitorFunctions{..}
  where
    walk = sendVisitorTDownPath
    step = stepVisitorTThroughCheckpoint
    run = id
getVisitorFunctions (ImpureVisitor run) = VisitorFunctions{..}
  where
    walk = sendVisitorTDownPath
    step = stepVisitorTThroughCheckpoint
-- }}}

genericForkVisitorTWorkerThread :: -- {{{
    VisitorMode α iv ip fv fp →
    VisitorKind m n →
    (WorkerTerminationReason fp → IO ()) →
    VisitorT m α →
    Workload →
    IO (WorkerEnvironment ip)
genericForkVisitorTWorkerThread
    visitor_mode
    visitor_kind
    finishedCallback
    visitor
    (Workload initial_path initial_checkpoint)
  = do
    -- Note:  the following line of code needs to be this way --- that is, using
    --        do notation to extract the value of VisitorFunctions --- or else
    --        GHC's head will explode!
    VisitorFunctions{..} ← case getVisitorFunctions visitor_kind of x → return x
    pending_requests_ref ← newIORef []
    let loop1 result cursor visitor_state = -- Check for requests {{{
            liftIO (readIORef pending_requests_ref) >>= \pending_requests →
            case pending_requests of
                [] → loop3 result cursor visitor_state
                _ → debugM "Worker thread's request queue is non-empty."
                    >> (liftM reverse . liftIO $ atomicModifyIORef pending_requests_ref (const [] &&& id))
                    >>= loop2 result cursor visitor_state
        -- }}}
        loop2 result cursor visitor_state@(VisitorTState context checkpoint visitor) requests = -- Process requests {{{
            case requests of
                [] → liftIO yield >> loop3 result cursor visitor_state
                AbortRequested:_ → do
                    debugM "Worker theread received abort request."
                    return WorkerAborted
                ProgressUpdateRequested submitProgress:rest_requests → do
                    debugM "Worker thread received progress update request."
                    liftIO . submitProgress $ computeProgressUpdate visitor_mode result initial_path cursor context checkpoint
                    loop2 (initialIntermediateValueOf visitor_mode) cursor visitor_state rest_requests
                WorkloadStealRequested submitMaybeWorkload:rest_requests → do
                    debugM "Worker thread received workload steal."
                    case tryStealWorkload initial_path cursor context of
                        Nothing → do
                            liftIO $ submitMaybeWorkload Nothing
                            loop2 result cursor visitor_state rest_requests
                        Just (new_cursor,new_context,workload) → do
                            liftIO . submitMaybeWorkload . Just $
                                StolenWorkload
                                    (computeProgressUpdate visitor_mode result initial_path new_cursor new_context checkpoint)
                                    workload
                            loop2 (initialIntermediateValueOf visitor_mode) new_cursor (VisitorTState new_context checkpoint visitor) rest_requests
        -- }}}
        loop3 result cursor visitor_state -- Step visitor {{{
          = do
            (maybe_solution,maybe_new_visitor_state) ← step visitor_state 
            case maybe_new_visitor_state of
                Nothing →
                    let explored_checkpoint =
                            checkpointFromInitialPath initial_path
                            .
                            checkpointFromCursor cursor
                            $
                            Explored
                    in return . WorkerFinished $
                    case visitor_mode of
                        AllMode →
                            Progress
                                explored_checkpoint
                                (maybe result (mappend result) maybe_solution)
                        FirstMode → maybe (Left explored_checkpoint) Right maybe_solution
                Just new_visitor_state →
                    case maybe_solution of
                        Nothing → loop1 result cursor new_visitor_state
                        Just solution →
                            case visitor_mode of
                                AllMode → do
                                    new_result ← liftIO . evaluate $ solution `seq` (result `mappend` solution)
                                    loop1 new_result cursor new_visitor_state
                                FirstMode → return . WorkerFinished . Right $ solution
        -- }}}
    start_flag_ivar ← IVar.new
    finished_flag ← IVar.new
    thread_id ← forkIO $ do
        termination_reason ←
            debugM "Worker thread has been forked." >>
            (run $
                walk initial_path visitor
                >>=
                loop1 (initialIntermediateValueOf visitor_mode) Seq.empty
                .
                initialVisitorState initial_checkpoint
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
{-# INLINE genericForkVisitorTWorkerThread #-}
-- }}}

genericRunVisitor :: -- {{{
    Monoid α ⇒
    ((WorkerTerminationReason α → IO ()) → Workload → IO (WorkerEnvironment α)) →
    IO (WorkerTerminationReason α)
genericRunVisitor forkWorkerThread = do
    result_mvar ← newEmptyMVar
    _ ← forkWorkerThread
            (putMVar result_mvar)
            entire_workload
    takeMVar result_mvar
-- }}}

runVisitor :: Monoid α ⇒ Visitor α → IO (WorkerTerminationReason α) -- {{{
runVisitor = liftM extractAllFinishedResult . genericRunVisitor . flip forkVisitorWorkerThread
-- }}}

runVisitorIO :: Monoid α ⇒ VisitorIO α → IO (WorkerTerminationReason α) -- {{{
runVisitorIO = liftM extractAllFinishedResult . genericRunVisitor . flip forkVisitorIOWorkerThread
-- }}}

runVisitorT :: (Monoid α, MonadIO m) ⇒ (∀ β. m β → IO β) → VisitorT m α → IO (WorkerTerminationReason α) -- {{{
runVisitorT runInIO = liftM extractAllFinishedResult . genericRunVisitor . flip (forkVisitorTWorkerThread runInIO)
-- }}}

sendAbortRequest :: WorkerRequestQueue ip → IO () -- {{{
sendAbortRequest = flip sendRequest AbortRequested
-- }}}

sendProgressUpdateRequest :: -- {{{
    WorkerRequestQueue ip →
    (ProgressUpdate ip → IO ()) →
    IO ()
sendProgressUpdateRequest queue = sendRequest queue . ProgressUpdateRequested
-- }}}

sendRequest :: WorkerRequestQueue ip → WorkerRequest ip → IO () -- {{{
sendRequest queue request = atomicModifyIORef queue ((request:) &&& const ())
-- }}}

sendWorkloadStealRequest :: -- {{{
    WorkerRequestQueue ip →
    (Maybe (StolenWorkload ip) → IO ()) →
    IO ()
sendWorkloadStealRequest queue = sendRequest queue . WorkloadStealRequested
-- }}}

tryStealWorkload :: -- {{{
    Path →
    CheckpointCursor →
    Context m α →
    Maybe (CheckpointCursor,Context m α,Workload)
tryStealWorkload initial_path = go
  where
    go _      (viewl → EmptyL) = Nothing
    go cursor (viewl → step :< rest_context) = case step of
        CacheContextStep cache →
            go (cursor |> CacheCheckpointD cache) rest_context
        LeftBranchContextStep other_checkpoint _ →
            Just
                (cursor |> ChoiceCheckpointD LeftBranch Unexplored
                ,rest_context
                ,Workload
                    ((initial_path >< pathFromCursor cursor) |> ChoiceStep RightBranch)
                    other_checkpoint
                )
        RightBranchContextStep →
            go (cursor |> ChoiceCheckpointD RightBranch Explored) rest_context
-- }}}

-- }}}
