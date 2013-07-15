{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

{-| This back-end implements parallelism by spawning multiple threads.  The
    number of threads can be changed during the run and even be set to zero.

    The driver provided by this back-end sets the number of threads equal to the
    number of capabilities as reported by 'getNumCapabilities';  that is, if you
    want @#@ parallel workers, then you need to pass @+RTS -N#@ as command-line
    arguments to tell the runtime that you want it to run @#@ threads in parallel.
 -}
module Visitor.Parallel.BackEnd.Threads
    (
    -- * Driver
      driver
    -- * Controller
    , ThreadsControllerMonad
    , abort
    , changeNumberOfWorkers
    , changeNumberOfWorkersAsync
    , changeNumberOfWorkersToMatchCapabilities
    , fork
    , getCurrentProgress
    , getCurrentProgressAsync
    , getNumberOfWorkers
    , getNumberOfWorkersAsync
    , requestProgressUpdate
    , requestProgressUpdateAsync
    -- * Visit functions
    -- $visit

    -- ** Sum over all results
    -- $all
    , visitTree
    , visitTreeStartingFrom
    , visitTreeIO
    , visitTreeIOStartingFrom
    , visitTreeT
    , visitTreeTStartingFrom
    -- ** Stop at first result
    -- $first
    , visitTreeUntilFirst
    , visitTreeUntilFirstStartingFrom
    , visitTreeIOUntilFirst
    , visitTreeIOUntilFirstStartingFrom
    , visitTreeTUntilFirst
    , visitTreeTUntilFirstStartingFrom
    -- ** Stop when sum of results meets condition
    -- $found

    -- *** Pull
    -- $pull
    , visitTreeUntilFoundUsingPull
    , visitTreeUntilFoundUsingPullStartingFrom
    , visitTreeIOUntilFoundUsingPull
    , visitTreeIOUntilFoundUsingPullStartingFrom
    , visitTreeTUntilFoundUsingPull
    , visitTreeTUntilFoundUsingPullStartingFrom
    -- *** Push
    -- $push
    , visitTreeUntilFoundUsingPush
    , visitTreeUntilFoundUsingPushStartingFrom
    , visitTreeIOUntilFoundUsingPush
    , visitTreeIOUntilFoundUsingPushStartingFrom
    , visitTreeTUntilFoundUsingPush
    , visitTreeTUntilFoundUsingPushStartingFrom
    -- * Generic runner
    , runVisitor
    ) where

import Control.Applicative (Applicative,liftA2)
import Control.Concurrent (getNumCapabilities,killThread)
import Control.Monad (void)
import Control.Monad.CatchIO (MonadCatchIO)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Control.Monad.Trans.State.Strict (get,modify)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(mempty))
import Data.Void (absurd)

import qualified System.Log.Logger as Logger
import System.Log.Logger (Priority(DEBUG))
import System.Log.Logger.TH

import Visitor (TreeGenerator,TreeGeneratorIO,TreeGeneratorT)
import Visitor.Checkpoint
import Visitor.Parallel.Main (Driver(..),DriverParameters(..),RunOutcome,RunOutcomeFor,mainParser)
import Visitor.Parallel.Common.ExplorationMode
import Visitor.Parallel.Common.Supervisor.RequestQueue
import Visitor.Parallel.Common.Worker as Worker
    hiding
    (visitTree
    ,visitTreeIO
    ,visitTreeT
    ,visitTreeUntilFirst
    ,visitTreeIOUntilFirst
    ,visitTreeTUntilFirst
    )
import Visitor.Parallel.Common.Workgroup hiding (C,unwrapC)

--------------------------------------------------------------------------------
----------------------------------- Loggers ------------------------------------
--------------------------------------------------------------------------------

deriveLoggers "Logger" [DEBUG]

--------------------------------------------------------------------------------
------------------------------------ Driver ------------------------------------
--------------------------------------------------------------------------------

{-| This is the driver for the threads back-end.  The number of workers is set
    to be equal to the number of runtime capabilities, which you set by using
    the @-N@ option for the RTS --- i.e., by specifying @+RTS -N#@ for @#@
    parallel workers (or just @+RTS -N@ to set the number of workers equal to
    the number of processors).
 -}
driver :: Driver IO shared_configuration supervisor_configuration m n exploration_mode
driver = Driver $ \DriverParameters{..} → do
    (shared_configuration,supervisor_configuration) ←
        mainParser (liftA2 (,) shared_configuration_term supervisor_configuration_term) program_info
    initializeGlobalState shared_configuration
    starting_progress ← getStartingProgress shared_configuration supervisor_configuration
    runVisitor
        (constructExplorationMode shared_configuration)
         purity
         starting_progress
        (constructTreeGenerator shared_configuration)
        (changeNumberOfWorkersToMatchCapabilities >> constructManager shared_configuration supervisor_configuration)
     >>= notifyTerminated shared_configuration supervisor_configuration

--------------------------------------------------------------------------------
---------------------------------- Controller ----------------------------------
--------------------------------------------------------------------------------

{-| This is the monad in which the thread controller will run. -}
newtype ThreadsControllerMonad exploration_mode α =
    C { unwrapC :: WorkgroupControllerMonad (IntMap (WorkerEnvironment (ProgressFor exploration_mode))) exploration_mode α
      } deriving (Applicative,Functor,Monad,MonadCatchIO,MonadIO,RequestQueueMonad,WorkgroupRequestQueueMonad)

instance HasExplorationMode (ThreadsControllerMonad exploration_mode) where
    type ExplorationModeFor (ThreadsControllerMonad exploration_mode) = exploration_mode

{-| Changes the number of a parallel workers to equal the number of capabilities
    as reported by 'getNumCapabilities'.
 -}
changeNumberOfWorkersToMatchCapabilities :: ThreadsControllerMonad exploration_mode ()
changeNumberOfWorkersToMatchCapabilities =
    liftIO getNumCapabilities >>= \n → changeNumberOfWorkersAsync (const (return n)) (void . return)

--------------------------------------------------------------------------------
------------------------------- Visit functions --------------------------------
--------------------------------------------------------------------------------

{- $visit
The functions in this section are provided as a way to use the threads back-end
directly rather than using the framework provided in "Visitor.Parallel.Main".
They are all specialized versions of 'runVisitor', which appears
in the following section; they are provided for convenience --- specifically, to
minimize the knowledge needed of the implementation and how the types specialize
for the various exploration modes.

There are 3 × 2 × 4 = 24 functions in this section; the factor of 3 comes from
the fact that there are three cases of monad in which the tree visitor is run:
pure, IO, and impure (where IO is a special case of impure provided for
convenience); the factor of 2 comes from the fact that one can either start with
no progress or start with a given progress; the factor of 4 comes from the fact
that there are four exploration modes: summing over all results, returning the first
result, summing over all results until a criteria is met with intermediate
results only being sent to the supervisor upon request, and the previous mode
but with all intermediate results being sent immediately to the supervisor.
 -}

---------------------------- Sum over all results ------------------------------

{- $all
The functions in this section are for when you want to sum over all the results
in (the leaves of) the tree.
 -}

{-| Visit the purely generated tree and sum over all results. -}
visitTree ::
    Monoid result ⇒
    TreeGenerator result {-^ the (pure) tree generator -} →
    ThreadsControllerMonad (AllMode result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    IO (RunOutcome (Progress result) result) {-^ the outcome of the run -}
visitTree = visitTreeStartingFrom mempty

{-| Like 'visitTree' but with a starting progress. -}
visitTreeStartingFrom ::
    Monoid result ⇒
    Progress result {-^ the starting progress -} →
    TreeGenerator result {-^ the (pure) tree generator -} →
    ThreadsControllerMonad (AllMode result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    IO (RunOutcome (Progress result) result) {-^ the outcome of the run -}
visitTreeStartingFrom = runVisitor AllMode Pure

{-| Like 'visitTree' but with the tree generator running in IO. -}
visitTreeIO ::
    Monoid result ⇒
    TreeGeneratorIO result {-^ the tree generator (which runs in the IO monad) -} →
    ThreadsControllerMonad (AllMode result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    IO (RunOutcome (Progress result) result) {-^ the outcome of the run -}
visitTreeIO = visitTreeIOStartingFrom mempty

{-| Like 'visitTreeIO' but with a starting progress. -}
visitTreeIOStartingFrom ::
    Monoid result ⇒
    Progress result {-^ the starting progress -} →
    TreeGeneratorIO result {-^ the tree generator (which runs in the IO monad) -} →
    ThreadsControllerMonad (AllMode result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    IO (RunOutcome (Progress result) result) {-^ the outcome of the run -}
visitTreeIOStartingFrom = runVisitor AllMode io_purity

{-| Like 'visitTree' but with a generic impure tree generator. -}
visitTreeT ::
    (Monoid result, MonadIO m) ⇒
    (∀ α. m α → IO α) {-^ the function that runs the tree generator's monad in IO -} →
    TreeGeneratorT m result {-^ the (impure) tree generator -} →
    ThreadsControllerMonad (AllMode result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    IO (RunOutcome (Progress result) result) {-^ the outcome of the run -}
visitTreeT = flip visitTreeTStartingFrom mempty

{-| Like 'visitTreeT', but with a starting progress. -}
visitTreeTStartingFrom ::
    (Monoid result, MonadIO m) ⇒
    (∀ α. m α → IO α) {-^ the function that runs the tree generator's monad in IO -} →
    Progress result {-^ the starting progress -} →
    TreeGeneratorT m result {-^ the (impure) tree generator -} →
    ThreadsControllerMonad (AllMode result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    IO (RunOutcome (Progress result) result)
visitTreeTStartingFrom = runVisitor AllMode  . ImpureAtopIO

---------------------------- Stop at first result ------------------------------

{- $first
See "Visitor.Parallel.Main#first" for more details on this mode.
 -}

{-| Visit the purely generated tree until a result has been found. -}
visitTreeUntilFirst ::
    TreeGenerator result {-^ the (pure) tree generator -} →
    ThreadsControllerMonad (FirstMode result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    IO (RunOutcome Checkpoint (Maybe (Progress result))) {-^ the outcome of the run -}
visitTreeUntilFirst = visitTreeUntilFirstStartingFrom mempty

{-| Like 'visitTreeUntilFirst' but with a starting progress. -}
visitTreeUntilFirstStartingFrom ::
    Checkpoint {-^ the starting progress -} →
    TreeGenerator result {-^ the (pure) tree generator -} →
    ThreadsControllerMonad (FirstMode result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    IO (RunOutcome Checkpoint (Maybe (Progress result))) {-^ the outcome of the run -}
visitTreeUntilFirstStartingFrom = runVisitor FirstMode Pure

{-| Like 'visitTreeUntilFirst' but with the tree generator running in IO. -}
visitTreeIOUntilFirst ::
    TreeGeneratorIO result {-^ the tree generator (which runs in the IO monad) -} →
    ThreadsControllerMonad (FirstMode result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    IO (RunOutcome Checkpoint (Maybe (Progress result))) {-^ the outcome of the run -}
visitTreeIOUntilFirst = visitTreeIOUntilFirstStartingFrom mempty

{-| Like 'visitTreeIOUntilFirst' but with a starting progress. -}
visitTreeIOUntilFirstStartingFrom ::
    Checkpoint {-^ the starting progress -} →
    TreeGeneratorIO result {-^ the tree generator (which runs in the IO monad) -} →
    ThreadsControllerMonad (FirstMode result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    IO (RunOutcome Checkpoint (Maybe (Progress result))) {-^ the outcome of the run -}
visitTreeIOUntilFirstStartingFrom = runVisitor FirstMode io_purity

{-| Like 'visitTreeUntilFirst' but with a generic impure tree generator. -}
visitTreeTUntilFirst ::
    MonadIO m ⇒
    (∀ α. m α → IO α) {-^ the function that runs the tree generator's monad in IO -} →
    TreeGeneratorT m result {-^ the (impure) tree generator -} →
    ThreadsControllerMonad (FirstMode result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    IO (RunOutcome Checkpoint (Maybe (Progress result))) {-^ the outcome of the run -}
visitTreeTUntilFirst = flip visitTreeTUntilFirstStartingFrom mempty

{-| Like 'visitTreeTUntilFirst', but with a starting progress. -}
visitTreeTUntilFirstStartingFrom ::
    MonadIO m ⇒
    (∀ α. m α → IO α) {-^ the function that runs the tree generator's monad in IO -} →
    Checkpoint {-^ the starting progress -} →
    TreeGeneratorT m result {-^ the (impure) tree generator -} →
    ThreadsControllerMonad (FirstMode result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    IO (RunOutcome Checkpoint (Maybe (Progress result))) {-^ the outcome of the run -}
visitTreeTUntilFirstStartingFrom = runVisitor FirstMode . ImpureAtopIO

------------------------ Stop when sum of results found ------------------------

{- $found
See "Visitor.Parallel.Main#found" (a direct hyper-link to the relevant section) for more information on this mode.
-}

{- $pull
See "Visitor.Parallel.Main#pull" (a direct hyper-link to the relevant section) for more information on this mode.

Note that because using these functions entails writing the controller yourself,
it is your responsibility to ensure that a global progress update is performed
on a regular basis in order to ensure that results are being gathered together
at the supervisor.
 -}

{-| Visit the purely generated tree until the sum of resuts meets a condition. -}
visitTreeUntilFoundUsingPull ::
    Monoid result ⇒
    (result → Maybe final_result) {-^ a condition function that signals when we have found all of the result that we wanted -} →
    TreeGenerator result {-^ the (pure) tree generator -} →
    ThreadsControllerMonad (FoundModeUsingPull result final_result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    IO (RunOutcome (Progress result) (Either result (Progress (final_result,result)))) {-^ the outcome of the run -}
visitTreeUntilFoundUsingPull = flip visitTreeUntilFoundUsingPullStartingFrom mempty

{-| Like 'visitTreeUntilFoundUsingPull' but with a starting progress. -}
visitTreeUntilFoundUsingPullStartingFrom ::
    Monoid result ⇒
    (result → Maybe final_result) →
    Progress result {-^ the starting progress -} →
    TreeGenerator result {-^ the (pure) tree generator -} →
    ThreadsControllerMonad (FoundModeUsingPull result final_result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    IO (RunOutcome (Progress result) (Either result (Progress (final_result,result)))) {-^ the outcome of the run -}
visitTreeUntilFoundUsingPullStartingFrom f = runVisitor (FoundModeUsingPull f) Pure

{-| Like 'visitTreeUntilFoundUsingPull' but with the tree generator running in IO. -}
visitTreeIOUntilFoundUsingPull ::
    Monoid result ⇒
    (result → Maybe final_result) →
    TreeGeneratorIO result {-^ the tree generator (which runs in the IO monad) -} →
    ThreadsControllerMonad (FoundModeUsingPull result final_result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    IO (RunOutcome (Progress result) (Either result (Progress (final_result,result)))) {-^ the outcome of the run -}
visitTreeIOUntilFoundUsingPull = flip visitTreeIOUntilFoundUsingPullStartingFrom mempty

{-| Like 'visitTreeIOUntilFoundUsingPull' but with a starting progress. -}
visitTreeIOUntilFoundUsingPullStartingFrom ::
    Monoid result ⇒
    (result → Maybe final_result) →
    Progress result {-^ the starting progress -} →
    TreeGeneratorIO result {-^ the tree generator (which runs in the IO monad) -} →
    ThreadsControllerMonad (FoundModeUsingPull result final_result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    IO (RunOutcome (Progress result) (Either result (Progress (final_result,result)))) {-^ the outcome of the run -}
visitTreeIOUntilFoundUsingPullStartingFrom f = runVisitor (FoundModeUsingPull f) io_purity

{-| Like 'visitTreeUntilFoundUsingPull' but with a generic impure tree generator. -}
visitTreeTUntilFoundUsingPull ::
    (Monoid result, MonadIO m) ⇒
    (result → Maybe final_result) →
    (∀ α. m α → IO α) {-^ the function that runs the tree generator's monad in IO -} →
    TreeGeneratorT m result {-^ the (impure) tree generator -} →
    ThreadsControllerMonad (FoundModeUsingPull result final_result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    IO (RunOutcome (Progress result) (Either result (Progress (final_result,result)))) {-^ the outcome of the run -}
visitTreeTUntilFoundUsingPull f run = visitTreeTUntilFoundUsingPullStartingFrom f run mempty

{-| Like 'visitTreeTUntilFoundUsingPull' but with a starting progress. -}
visitTreeTUntilFoundUsingPullStartingFrom ::
    (Monoid result, MonadIO m) ⇒
    (result → Maybe final_result) →
    (∀ α. m α → IO α) {-^ the function that runs the tree generator's monad in IO -} →
    Progress result {-^ the starting progress -} →
    TreeGeneratorT m result {-^ the (impure) tree generator -} →
    ThreadsControllerMonad (FoundModeUsingPull result final_result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    IO (RunOutcome (Progress result) (Either result (Progress (final_result,result)))) {-^ the outcome of the run -}
visitTreeTUntilFoundUsingPullStartingFrom f = runVisitor (FoundModeUsingPull f) . ImpureAtopIO

{- $push
See "Visitor.Parallel.Main#push" (a direct hyper-link to the relevant section) for more information on this mode.
-}


{-| Visit the purely generated tree until the sum of resuts meets a condition. -}
visitTreeUntilFoundUsingPush ::
    Monoid result ⇒
    (result → Maybe final_result) →
    TreeGenerator result {-^ the (pure) tree generator -} →
    ThreadsControllerMonad (FoundModeUsingPush result final_result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    IO (RunOutcome (Progress result) (Either result (Progress final_result))) {-^ the outcome of the run -}
visitTreeUntilFoundUsingPush = flip visitTreeUntilFoundUsingPushStartingFrom mempty

{-| Like 'visitTreeUntilFoundUsingPush', but with a starting result. -}
visitTreeUntilFoundUsingPushStartingFrom ::
    Monoid result ⇒
    (result → Maybe final_result) →
    Progress result {-^ the starting progress -} →
    TreeGenerator result {-^ the (pure) tree generator -} →
    ThreadsControllerMonad (FoundModeUsingPush result final_result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    IO (RunOutcome (Progress result) (Either result (Progress final_result))) {-^ the outcome of the run -}
visitTreeUntilFoundUsingPushStartingFrom f = runVisitor (FoundModeUsingPush f) Pure

{-| Like 'visitTreeUntilFoundUsingPush' but with the tree generator running in IO. -}
visitTreeIOUntilFoundUsingPush ::
    Monoid result ⇒
    (result → Maybe final_result) →
    TreeGeneratorIO result {-^ the tree generator (which runs in the IO monad) -} →
    ThreadsControllerMonad (FoundModeUsingPush result final_result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    IO (RunOutcome (Progress result) (Either result (Progress final_result))) {-^ the outcome of the run -}
visitTreeIOUntilFoundUsingPush = flip visitTreeIOUntilFoundUsingPushStartingFrom mempty

{-| Like 'visitTreeIOUntilFoundUsingPush', but with a starting result. -}
visitTreeIOUntilFoundUsingPushStartingFrom ::
    Monoid result ⇒
    (result → Maybe final_result) →
    Progress result {-^ the starting progress -} →
    TreeGeneratorIO result {-^ the tree generator (which runs in the IO monad) -} →
    ThreadsControllerMonad (FoundModeUsingPush result final_result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    IO (RunOutcome (Progress result) (Either result (Progress final_result))) {-^ the outcome of the run -}
visitTreeIOUntilFoundUsingPushStartingFrom f = runVisitor (FoundModeUsingPush f) io_purity

{-| Like 'visitTreeUntilFoundUsingPush' but with a generic impure tree generator. -}
visitTreeTUntilFoundUsingPush ::
    (Monoid result, MonadIO m) ⇒
    (result → Maybe final_result) →
    (∀ α. m α → IO α) {-^ the function that runs the tree generator's monad in IO -} →
    TreeGeneratorT m result {-^ the (impure) tree generator -} →
    ThreadsControllerMonad (FoundModeUsingPush result final_result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    IO (RunOutcome (Progress result) (Either result (Progress final_result))) {-^ the outcome of the run -}
visitTreeTUntilFoundUsingPush f run = visitTreeTUntilFoundUsingPushStartingFrom f run mempty

{-| Like 'visitTreeTUntilFoundUsingPush', but with a starting progress. -}
visitTreeTUntilFoundUsingPushStartingFrom ::
    (Monoid result, MonadIO m) ⇒
    (result → Maybe final_result) →
    (∀ α. m α → IO α) {-^ the function that runs the tree generator's monad in IO -} →
    Progress result {-^ the starting progress -} →
    TreeGeneratorT m result {-^ the (impure) tree generator -} →
    ThreadsControllerMonad (FoundModeUsingPush result final_result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    IO (RunOutcome (Progress result) (Either result (Progress final_result))) {-^ the outcome of the run -}
visitTreeTUntilFoundUsingPushStartingFrom f = runVisitor (FoundModeUsingPush f) . ImpureAtopIO

--------------------------------------------------------------------------------
-------------------------------- Generic runner --------------------------------
--------------------------------------------------------------------------------

{-| Visits the given tree using multiple threads to achieve parallelism.

    This function grants access to all of the functionality of this back-end,
    but because its generality complicates its use (primarily the fact that the
    types are dependent on the first parameter) you may find it easier to use
    one of the specialized functions in the preceding section.
 -}
runVisitor ::
    ExplorationMode exploration_mode {-^ the exploration mode -} →
    Purity m n {-^ the purity of the tree generator -} →
    (ProgressFor exploration_mode) {-^ the starting progress -} →
    TreeGeneratorT m (ResultFor exploration_mode) {-^ the tree generator -} →
    ThreadsControllerMonad exploration_mode () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    IO (RunOutcomeFor exploration_mode) {-^ the outcome of the run -}
runVisitor exploration_mode purity starting_progress visitor (C controller) =
    runWorkgroup
        exploration_mode
        mempty
        (\MessageForSupervisorReceivers{..} →
            let createWorker _ = return ()
                destroyWorker worker_id False = liftIO $ receiveQuitFromWorker worker_id
                destroyWorker worker_id True = do
                    get >>=
                        liftIO
                        .
                        sendAbortRequest
                        .
                        workerPendingRequests
                        .
                        fromJustOrBust ("destroyWorker: active record for worker " ++ show worker_id ++ " not found")
                        .
                        IntMap.lookup worker_id
                    modify (IntMap.delete worker_id)

                killAllWorkers _ =
                    get >>=
                        liftIO
                        .
                        mapM_ (killThread . workerThreadId)
                        .
                        IntMap.elems

                sendRequestToWorker request receiver worker_id =
                    get >>=
                        liftIO
                        .
                        maybe (return ()) (
                            flip request (receiver worker_id)
                            .
                            workerPendingRequests
                        )
                        .
                        IntMap.lookup worker_id

                sendProgressUpdateRequestTo = sendRequestToWorker Worker.sendProgressUpdateRequest receiveProgressUpdateFromWorker
                sendWorkloadStealRequestTo = sendRequestToWorker Worker.sendWorkloadStealRequest receiveStolenWorkloadFromWorker
                sendWorkloadTo worker_id workload =
                    (debugM $ "Sending " ++ show workload ++ " to worker " ++ show worker_id)
                    >>
                    (liftIO $
                        forkWorkerThread
                            exploration_mode
                            purity
                            (\termination_reason →
                                case termination_reason of
                                    WorkerFinished final_progress →
                                        receiveFinishedFromWorker worker_id final_progress
                                    WorkerFailed message →
                                        receiveFailureFromWorker worker_id message
                                    WorkerAborted →
                                        receiveQuitFromWorker worker_id
                            )
                            visitor
                            workload
                            (case exploration_mode of
                                AllMode → absurd
                                FirstMode → absurd
                                FoundModeUsingPull _ → absurd
                                FoundModeUsingPush _ → receiveProgressUpdateFromWorker worker_id
                            )
                    )
                    >>=
                    modify
                    .
                    IntMap.insert worker_id
                    >>
                    (debugM $ "Thread for worker " ++ show worker_id ++ "started.")

            in WorkgroupCallbacks{..}
        )
        starting_progress
        controller

--------------------------------------------------------------------------------
----------------------------------- Internal -----------------------------------
--------------------------------------------------------------------------------

fromJustOrBust :: String → Maybe α → α
fromJustOrBust message = fromMaybe (error message)
