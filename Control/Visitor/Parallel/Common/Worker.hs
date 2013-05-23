-- Language extensions {{{
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
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
    , ProgressUpdateFor
    , StolenWorkload(..)
    , StolenWorkloadFor
    , WorkerRequestQueue
    , WorkerRequestQueueFor
    , WorkerEnvironment(..)
    , WorkerEnvironmentFor
    , WorkerTerminationReason(..)
    , WorkerTerminationReasonFor
    , VisitorKind(..)
    , forkWorkerThread
    , genericRunVisitor
    , runVisitor
    , runVisitorIO
    , runVisitorT
    , searchVisitor
    , searchVisitorIO
    , searchVisitorT
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
import Data.Functor ((<$>))
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

import Control.Visitor hiding (runVisitor,runVisitorT,searchVisitor,searchVisitorT)
import Control.Visitor.Checkpoint
import Control.Visitor.Parallel.Common.VisitorMode
import Control.Visitor.Path
import Control.Visitor.Workload
-- }}}

-- Types {{{

data ProgressUpdate progress = ProgressUpdate -- {{{
    {   progressUpdateProgress :: progress
    ,   progressUpdateRemainingWorkload :: Workload
    } deriving (Eq,Show)
$( derive makeSerialize ''ProgressUpdate )
-- }}}
type ProgressUpdateFor visitor_mode = ProgressUpdate (ProgressFor visitor_mode)

data StolenWorkload progress = StolenWorkload -- {{{
    {   stolenWorkloadProgressUpdate :: ProgressUpdate progress
    ,   stolenWorkload :: Workload
    } deriving (Eq,Show)
$( derive makeSerialize ''StolenWorkload )
-- }}}
type StolenWorkloadFor visitor_mode = StolenWorkload (ProgressFor visitor_mode)

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

data WorkerRequest progress = -- {{{
    AbortRequested
  | ProgressUpdateRequested (ProgressUpdate progress → IO ())
  | WorkloadStealRequested (Maybe (StolenWorkload progress) → IO ())
-- }}}
type WorkerRequestFor visitor_mode = WorkerRequest (ProgressFor visitor_mode)

type WorkerRequestQueue progress = IORef [WorkerRequest progress]
type WorkerRequestQueueFor visitor_mode = WorkerRequestQueue (ProgressFor visitor_mode)

data WorkerEnvironment progress = WorkerEnvironment -- {{{
    {   workerInitialPath :: Path
    ,   workerThreadId :: ThreadId
    ,   workerPendingRequests :: WorkerRequestQueue progress
    ,   workerTerminationFlag :: IVar ()
    }
-- }}}
type WorkerEnvironmentFor visitor_mode = WorkerEnvironment (ProgressFor visitor_mode)

data WorkerTerminationReason worker_final_progress = -- {{{
    WorkerFinished worker_final_progress
  | WorkerFailed String
  | WorkerAborted
  deriving (Eq,Show)
-- }}}
type WorkerTerminationReasonFor visitor_mode = WorkerTerminationReason (WorkerFinalProgressFor visitor_mode)

-- }}}

-- Logging Functions {{{
deriveLoggers "Logger" [DEBUG,INFO]
-- }}}

-- Functions {{{

computeProgressUpdate :: -- {{{
    ( VisitorMode visitor_mode
    , ResultFor visitor_mode ~ α
    ) ⇒
    visitor_mode →
    WorkerIntermediateValueFor visitor_mode →
    Path →
    CheckpointCursor →
    Context m α →
    Checkpoint →
    ProgressUpdate (ProgressFor visitor_mode)
computeProgressUpdate visitor_mode result initial_path cursor context checkpoint =
    ProgressUpdate
        (progressFrom visitor_mode result
         .
         checkpointFromInitialPath initial_path
         .
         checkpointFromCursor cursor
         .
         checkpointFromContext context
         $
         checkpoint
        )
        (Workload (initial_path >< pathFromCursor cursor)
         .
         checkpointFromContext context
         $
         checkpoint
        )
-- }}}

forkWorkerThread :: -- {{{
    (VisitorMode visitor_mode, ResultFor visitor_mode ~ α) ⇒
    visitor_mode →
    VisitorKind m n →
    (WorkerTerminationReasonFor visitor_mode → IO ()) →
    VisitorT m α →
    Workload →
    IO (WorkerEnvironmentFor visitor_mode)
forkWorkerThread
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
    let loop1 (!result) cursor visitor_state = -- Check for requests {{{
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
                    loop2 initial_intermediate_value cursor visitor_state rest_requests
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
                            loop2 initial_intermediate_value new_cursor (VisitorTState new_context checkpoint visitor) rest_requests
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
                        constructWorkerFinishedProgress
                            visitor_mode
                            result
                            maybe_solution
                            explored_checkpoint
                Just new_visitor_state →
                    case maybe_solution of
                        Nothing → loop1 result cursor new_visitor_state
                        Just (!solution) →
                            reactToSolutionFound visitor_mode result solution
                                (\new_result → loop1 new_result cursor new_visitor_state)
                                (return . WorkerFinished)
        -- }}}
    start_flag_ivar ← IVar.new
    finished_flag ← IVar.new
    thread_id ← forkIO $ do
        termination_reason ←
            debugM "Worker thread has been forked." >>
            (run $
                walk initial_path visitor
                >>=
                loop1 initial_intermediate_value Seq.empty
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
  where
    initial_intermediate_value = initialWorkerIntermediateValue visitor_mode
{-# INLINE forkWorkerThread #-}
-- }}}

genericRunVisitor :: -- {{{
    (VisitorMode visitor_mode, ResultFor visitor_mode ~ α) ⇒
    visitor_mode →
    VisitorKind m n →
    VisitorT m α →
    IO (WorkerTerminationReason (FinalResultFor visitor_mode))
genericRunVisitor visitor_mode visitor_kind visitor = do
    final_progress_mvar ← newEmptyMVar
    _ ← forkWorkerThread
            visitor_mode
            visitor_kind
            (putMVar final_progress_mvar)
            visitor
            entire_workload
    final_progress ← takeMVar final_progress_mvar
    return $ case final_progress of
        WorkerAborted → WorkerAborted
        WorkerFailed message → WorkerFailed message
        WorkerFinished progress → WorkerFinished $
            extractFinalValueFromFinalProgress visitor_mode progress
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
{-# INLINE getVisitorFunctions #-}
-- }}}

runVisitor :: Monoid α ⇒ Visitor α → IO (WorkerTerminationReason α) -- {{{
runVisitor = genericRunVisitor AllMode PureVisitor
-- }}}

runVisitorIO :: Monoid α ⇒ VisitorIO α → IO (WorkerTerminationReason α) -- {{{
runVisitorIO = genericRunVisitor AllMode IOVisitor
-- }}}

runVisitorT :: (Monoid α, MonadIO m) ⇒ (∀ β. m β → IO β) → VisitorT m α → IO (WorkerTerminationReason α) -- {{{
runVisitorT = genericRunVisitor AllMode . ImpureVisitor
-- }}}

searchVisitor :: Visitor α → IO (WorkerTerminationReason (Maybe α)) -- {{{
searchVisitor = genericRunVisitor FirstMode PureVisitor
-- }}}

searchVisitorIO :: VisitorIO α → IO (WorkerTerminationReason (Maybe α)) -- {{{
searchVisitorIO = genericRunVisitor FirstMode IOVisitor
-- }}}

searchVisitorT ::MonadIO m ⇒ (∀ β. m β → IO β) → VisitorT m α → IO (WorkerTerminationReason (Maybe α)) -- {{{
searchVisitorT = genericRunVisitor FirstMode . ImpureVisitor
-- }}}

sendAbortRequest :: WorkerRequestQueue progress → IO () -- {{{
sendAbortRequest = flip sendRequest AbortRequested
-- }}}

sendProgressUpdateRequest :: -- {{{
    WorkerRequestQueue progress →
    (ProgressUpdate progress → IO ()) →
    IO ()
sendProgressUpdateRequest queue = sendRequest queue . ProgressUpdateRequested
-- }}}

sendRequest :: WorkerRequestQueue progress → WorkerRequest progress → IO () -- {{{
sendRequest queue request = atomicModifyIORef queue ((request:) &&& const ())
-- }}}

sendWorkloadStealRequest :: -- {{{
    WorkerRequestQueue progress →
    (Maybe (StolenWorkload progress) → IO ()) →
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
