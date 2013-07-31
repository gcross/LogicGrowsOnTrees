{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

{-| This adapter implements parallelism by spawning multiple threads.  The
    number of threads can be changed during the run and even be set to zero.

    The driver provided by this adapter sets the number of threads equal to the
    number of capabilities as reported by 'getNumCapabilities';  that is, if you
    want @#@ parallel workers, then you need to pass @+RTS -N#@ as command-line
    arguments to tell the runtime that you want it to run @#@ threads in parallel.
 -}
module LogicGrowsOnTrees.Parallel.Adapter.Threads
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
    -- * Outcome types
    , RunOutcome(..)
    , RunStatistics(..)
    , TerminationReason(..)
    -- * Exploration functions
    -- $exploration

    -- ** Sum over all results
    -- $all
    , exploreTree
    , exploreTreeStartingFrom
    , exploreTreeIO
    , exploreTreeIOStartingFrom
    , exploreTreeT
    , exploreTreeTStartingFrom
    -- ** Stop at first result
    -- $first
    , exploreTreeUntilFirst
    , exploreTreeUntilFirstStartingFrom
    , exploreTreeIOUntilFirst
    , exploreTreeIOUntilFirstStartingFrom
    , exploreTreeTUntilFirst
    , exploreTreeTUntilFirstStartingFrom
    -- ** Stop when sum of results meets condition
    -- $found

    -- *** Pull
    -- $pull
    , exploreTreeUntilFoundUsingPull
    , exploreTreeUntilFoundUsingPullStartingFrom
    , exploreTreeIOUntilFoundUsingPull
    , exploreTreeIOUntilFoundUsingPullStartingFrom
    , exploreTreeTUntilFoundUsingPull
    , exploreTreeTUntilFoundUsingPullStartingFrom
    -- *** Push
    -- $push
    , exploreTreeUntilFoundUsingPush
    , exploreTreeUntilFoundUsingPushStartingFrom
    , exploreTreeIOUntilFoundUsingPush
    , exploreTreeIOUntilFoundUsingPushStartingFrom
    , exploreTreeTUntilFoundUsingPush
    , exploreTreeTUntilFoundUsingPushStartingFrom
    -- * Generic explorer
    , runExplorer
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

import LogicGrowsOnTrees (Tree,TreeIO,TreeT)
import LogicGrowsOnTrees.Checkpoint
import LogicGrowsOnTrees.Parallel.Main
    (Driver(..)
    ,DriverParameters(..)
    ,RunOutcome(..)
    ,RunOutcomeFor
    ,RunStatistics(..)
    ,TerminationReason(..)
    ,mainParser
    )
import LogicGrowsOnTrees.Parallel.Common.RequestQueue
import LogicGrowsOnTrees.Parallel.Common.Worker
import LogicGrowsOnTrees.Parallel.Common.Workgroup hiding (C,unwrapC)
import LogicGrowsOnTrees.Parallel.ExplorationMode
import LogicGrowsOnTrees.Parallel.Purity

--------------------------------------------------------------------------------
----------------------------------- Loggers ------------------------------------
--------------------------------------------------------------------------------

deriveLoggers "Logger" [DEBUG]

--------------------------------------------------------------------------------
------------------------------------ Driver ------------------------------------
--------------------------------------------------------------------------------

{-| This is the driver for the threads adapter.  The number of workers is set
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
    runExplorer
        (constructExplorationMode shared_configuration)
         purity
         starting_progress
        (changeNumberOfWorkersToMatchCapabilities >> constructManager shared_configuration supervisor_configuration)
        (constructTree shared_configuration)
     >>= notifyTerminated shared_configuration supervisor_configuration

--------------------------------------------------------------------------------
---------------------------------- Controller ----------------------------------
--------------------------------------------------------------------------------

{-| This is the monad in which the thread controller will run. -}
newtype ThreadsControllerMonad exploration_mode α =
    C (WorkgroupControllerMonad (IntMap (WorkerEnvironment (ProgressFor exploration_mode))) exploration_mode α)
  deriving (Applicative,Functor,Monad,MonadCatchIO,MonadIO,RequestQueueMonad,WorkgroupRequestQueueMonad)

instance HasExplorationMode (ThreadsControllerMonad exploration_mode) where
    type ExplorationModeFor (ThreadsControllerMonad exploration_mode) = exploration_mode

{-| Changes the number of a parallel workers to equal the number of capabilities
    as reported by 'getNumCapabilities'.
 -}
changeNumberOfWorkersToMatchCapabilities :: ThreadsControllerMonad exploration_mode ()
changeNumberOfWorkersToMatchCapabilities =
    liftIO getNumCapabilities >>= \n → changeNumberOfWorkersAsync (const . return . fromIntegral $ n) (void . return)

--------------------------------------------------------------------------------
---------------------------- Exploration functions -----------------------------
--------------------------------------------------------------------------------

{- $exploration
The functions in this section are provided as a way to use the threads adapter
directly rather than using the framework provided in "LogicGrowsOnTrees.Parallel.Main".
They are all specialized versions of 'runExplorer', which appears
in the following section; they are provided for convenience --- specifically, to
minimize the knowledge needed of the implementation and how the types specialize
for the various exploration modes.

There are 3 × 2 × 4 = 24 functions in this section; the factor of 3 comes from
the fact that there are three cases of monad in which the exploration is run:
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

{-| Explore the pure tree and sum over all results. -}
exploreTree ::
    Monoid result ⇒
    ThreadsControllerMonad (AllMode result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    Tree result {-^ the (pure) tree -} →
    IO (RunOutcome (Progress result) result) {-^ the outcome of the run -}
exploreTree = exploreTreeStartingFrom mempty

{-| Like 'exploreTree' but with a starting progress. -}
exploreTreeStartingFrom ::
    Monoid result ⇒
    Progress result {-^ the starting progress -} →
    ThreadsControllerMonad (AllMode result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    Tree result {-^ the (pure) tree -} →
    IO (RunOutcome (Progress result) result) {-^ the outcome of the run -}
exploreTreeStartingFrom = runExplorer AllMode Pure

{-| Like 'exploreTree' but with the tree running in IO. -}
exploreTreeIO ::
    Monoid result ⇒
    ThreadsControllerMonad (AllMode result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    TreeIO result {-^ the tree (which runs in the IO monad) -} →
    IO (RunOutcome (Progress result) result) {-^ the outcome of the run -}
exploreTreeIO = exploreTreeIOStartingFrom mempty

{-| Like 'exploreTreeIO' but with a starting progress. -}
exploreTreeIOStartingFrom ::
    Monoid result ⇒
    Progress result {-^ the starting progress -} →
    ThreadsControllerMonad (AllMode result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    TreeIO result {-^ the tree (which runs in the IO monad) -} →
    IO (RunOutcome (Progress result) result) {-^ the outcome of the run -}
exploreTreeIOStartingFrom = runExplorer AllMode io_purity

{-| Like 'exploreTree' but with a generic impure tree. -}
exploreTreeT ::
    (Monoid result, MonadIO m) ⇒
    (∀ α. m α → IO α) {-^ the function that runs the tree's monad in IO -} →
    ThreadsControllerMonad (AllMode result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    TreeT m result {-^ the (impure) tree -} →
    IO (RunOutcome (Progress result) result) {-^ the outcome of the run -}
exploreTreeT = flip exploreTreeTStartingFrom mempty

{-| Like 'exploreTreeT', but with a starting progress. -}
exploreTreeTStartingFrom ::
    (Monoid result, MonadIO m) ⇒
    (∀ α. m α → IO α) {-^ the function that runs the tree's monad in IO -} →
    Progress result {-^ the starting progress -} →
    ThreadsControllerMonad (AllMode result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    TreeT m result {-^ the (impure) tree -} →
    IO (RunOutcome (Progress result) result)
exploreTreeTStartingFrom = runExplorer AllMode  . ImpureAtopIO

---------------------------- Stop at first result ------------------------------

{- $first
See "LogicGrowsOnTrees.Parallel.Main#first" for more details on this mode.
 -}

{-| Explore the pure tree until a result has been found. -}
exploreTreeUntilFirst ::
    ThreadsControllerMonad (FirstMode result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    Tree result {-^ the (pure) tree -} →
    IO (RunOutcome Checkpoint (Maybe (Progress result))) {-^ the outcome of the run -}
exploreTreeUntilFirst = exploreTreeUntilFirstStartingFrom mempty

{-| Like 'exploreTreeUntilFirst' but with a starting progress. -}
exploreTreeUntilFirstStartingFrom ::
    Checkpoint {-^ the starting progress -} →
    ThreadsControllerMonad (FirstMode result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    Tree result {-^ the (pure) tree -} →
    IO (RunOutcome Checkpoint (Maybe (Progress result))) {-^ the outcome of the run -}
exploreTreeUntilFirstStartingFrom = runExplorer FirstMode Pure

{-| Like 'exploreTreeUntilFirst' but with the tree running in IO. -}
exploreTreeIOUntilFirst ::
    ThreadsControllerMonad (FirstMode result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    TreeIO result {-^ the tree (which runs in the IO monad) -} →
    IO (RunOutcome Checkpoint (Maybe (Progress result))) {-^ the outcome of the run -}
exploreTreeIOUntilFirst = exploreTreeIOUntilFirstStartingFrom mempty

{-| Like 'exploreTreeIOUntilFirst' but with a starting progress. -}
exploreTreeIOUntilFirstStartingFrom ::
    Checkpoint {-^ the starting progress -} →
    ThreadsControllerMonad (FirstMode result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    TreeIO result {-^ the tree (which runs in the IO monad) -} →
    IO (RunOutcome Checkpoint (Maybe (Progress result))) {-^ the outcome of the run -}
exploreTreeIOUntilFirstStartingFrom = runExplorer FirstMode io_purity

{-| Like 'exploreTreeUntilFirst' but with a generic impure tree. -}
exploreTreeTUntilFirst ::
    MonadIO m ⇒
    (∀ α. m α → IO α) {-^ the function that runs the tree's monad in IO -} →
    ThreadsControllerMonad (FirstMode result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    TreeT m result {-^ the (impure) tree -} →
    IO (RunOutcome Checkpoint (Maybe (Progress result))) {-^ the outcome of the run -}
exploreTreeTUntilFirst = flip exploreTreeTUntilFirstStartingFrom mempty

{-| Like 'exploreTreeTUntilFirst', but with a starting progress. -}
exploreTreeTUntilFirstStartingFrom ::
    MonadIO m ⇒
    (∀ α. m α → IO α) {-^ the function that runs the tree's monad in IO -} →
    Checkpoint {-^ the starting progress -} →
    ThreadsControllerMonad (FirstMode result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    TreeT m result {-^ the (impure) tree -} →
    IO (RunOutcome Checkpoint (Maybe (Progress result))) {-^ the outcome of the run -}
exploreTreeTUntilFirstStartingFrom = runExplorer FirstMode . ImpureAtopIO

------------------------ Stop when sum of results found ------------------------

{- $found
See "LogicGrowsOnTrees.Parallel.Main#found" (a direct hyper-link to the relevant section) for more information on this mode.
-}

{- $pull
See "LogicGrowsOnTrees.Parallel.Main#pull" (a direct hyper-link to the relevant section) for more information on this mode.

Note that because using these functions entails writing the controller yourself,
it is your responsibility to ensure that a global progress update is performed
on a regular basis in order to ensure that results are being gathered together
at the supervisor.
 -}

{-| Explore the pure tree until the sum of resuts meets a condition. -}
exploreTreeUntilFoundUsingPull ::
    Monoid result ⇒
    (result → Bool) {-^ a condition function that signals when we have found all of the result that we wanted -} →
    ThreadsControllerMonad (FoundModeUsingPull result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    Tree result {-^ the (pure) tree -} →
    IO (RunOutcome (Progress result) (Either result (Progress result))) {-^ the outcome of the run -}
exploreTreeUntilFoundUsingPull = flip exploreTreeUntilFoundUsingPullStartingFrom mempty

{-| Like 'exploreTreeUntilFoundUsingPull' but with a starting progress. -}
exploreTreeUntilFoundUsingPullStartingFrom ::
    Monoid result ⇒
    (result → Bool) {-^ a condition function that signals when we have found all of the result that we wanted -} →
    Progress result {-^ the starting progress -} →
    ThreadsControllerMonad (FoundModeUsingPull result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    Tree result {-^ the (pure) tree -} →
    IO (RunOutcome (Progress result) (Either result (Progress result))) {-^ the outcome of the run -}
exploreTreeUntilFoundUsingPullStartingFrom f = runExplorer (FoundModeUsingPull f) Pure

{-| Like 'exploreTreeUntilFoundUsingPull' but with the tree running in IO. -}
exploreTreeIOUntilFoundUsingPull ::
    Monoid result ⇒
    (result → Bool) {-^ a condition function that signals when we have found all of the result that we wanted -} →
    ThreadsControllerMonad (FoundModeUsingPull result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    TreeIO result {-^ the tree (which runs in the IO monad) -} →
    IO (RunOutcome (Progress result) (Either result (Progress result))) {-^ the outcome of the run -}
exploreTreeIOUntilFoundUsingPull = flip exploreTreeIOUntilFoundUsingPullStartingFrom mempty

{-| Like 'exploreTreeIOUntilFoundUsingPull' but with a starting progress. -}
exploreTreeIOUntilFoundUsingPullStartingFrom ::
    Monoid result ⇒
    (result → Bool) {-^ a condition function that signals when we have found all of the result that we wanted -} →
    Progress result {-^ the starting progress -} →
    ThreadsControllerMonad (FoundModeUsingPull result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    TreeIO result {-^ the tree (which runs in the IO monad) -} →
    IO (RunOutcome (Progress result) (Either result (Progress result))) {-^ the outcome of the run -}
exploreTreeIOUntilFoundUsingPullStartingFrom f = runExplorer (FoundModeUsingPull f) io_purity

{-| Like 'exploreTreeUntilFoundUsingPull' but with a generic impure tree. -}
exploreTreeTUntilFoundUsingPull ::
    (Monoid result, MonadIO m) ⇒
    (result → Bool) {-^ a condition function that signals when we have found all of the result that we wanted -} →
    (∀ α. m α → IO α) {-^ the function that runs the tree's monad in IO -} →
    ThreadsControllerMonad (FoundModeUsingPull result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    TreeT m result {-^ the (impure) tree -} →
    IO (RunOutcome (Progress result) (Either result (Progress result))) {-^ the outcome of the run -}
exploreTreeTUntilFoundUsingPull f run = exploreTreeTUntilFoundUsingPullStartingFrom f run mempty

{-| Like 'exploreTreeTUntilFoundUsingPull' but with a starting progress. -}
exploreTreeTUntilFoundUsingPullStartingFrom ::
    (Monoid result, MonadIO m) ⇒
    (result → Bool) {-^ a condition function that signals when we have found all of the result that we wanted -} →
    (∀ α. m α → IO α) {-^ the function that runs the tree's monad in IO -} →
    Progress result {-^ the starting progress -} →
    ThreadsControllerMonad (FoundModeUsingPull result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    TreeT m result {-^ the (impure) tree -} →
    IO (RunOutcome (Progress result) (Either result (Progress result))) {-^ the outcome of the run -}
exploreTreeTUntilFoundUsingPullStartingFrom f = runExplorer (FoundModeUsingPull f) . ImpureAtopIO

{- $push
See "LogicGrowsOnTrees.Parallel.Main#push" (a direct hyper-link to the relevant section) for more information on this mode.
-}


{-| Explore the pure tree until the sum of resuts meets a condition. -}
exploreTreeUntilFoundUsingPush ::
    Monoid result ⇒
    (result → Bool) {-^ a condition function that signals when we have found all of the result that we wanted -} →
    ThreadsControllerMonad (FoundModeUsingPush result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    Tree result {-^ the (pure) tree -} →
    IO (RunOutcome (Progress result) (Either result (Progress result))) {-^ the outcome of the run -}
exploreTreeUntilFoundUsingPush = flip exploreTreeUntilFoundUsingPushStartingFrom mempty

{-| Like 'exploreTreeUntilFoundUsingPush', but with a starting result. -}
exploreTreeUntilFoundUsingPushStartingFrom ::
    Monoid result ⇒
    (result → Bool) {-^ a condition function that signals when we have found all of the result that we wanted -} →
    Progress result {-^ the starting progress -} →
    ThreadsControllerMonad (FoundModeUsingPush result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    Tree result {-^ the (pure) tree -} →
    IO (RunOutcome (Progress result) (Either result (Progress result))) {-^ the outcome of the run -}
exploreTreeUntilFoundUsingPushStartingFrom f = runExplorer (FoundModeUsingPush f) Pure

{-| Like 'exploreTreeUntilFoundUsingPush' but with the tree running in IO. -}
exploreTreeIOUntilFoundUsingPush ::
    Monoid result ⇒
    (result → Bool) {-^ a condition function that signals when we have found all of the result that we wanted -} →
    ThreadsControllerMonad (FoundModeUsingPush result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    TreeIO result {-^ the tree (which runs in the IO monad) -} →
    IO (RunOutcome (Progress result) (Either result (Progress result))) {-^ the outcome of the run -}
exploreTreeIOUntilFoundUsingPush = flip exploreTreeIOUntilFoundUsingPushStartingFrom mempty

{-| Like 'exploreTreeIOUntilFoundUsingPush', but with a starting result. -}
exploreTreeIOUntilFoundUsingPushStartingFrom ::
    Monoid result ⇒
    (result → Bool) {-^ a condition function that signals when we have found all of the result that we wanted -} →
    Progress result {-^ the starting progress -} →
    ThreadsControllerMonad (FoundModeUsingPush result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    TreeIO result {-^ the tree (which runs in the IO monad) -} →
    IO (RunOutcome (Progress result) (Either result (Progress result))) {-^ the outcome of the run -}
exploreTreeIOUntilFoundUsingPushStartingFrom f = runExplorer (FoundModeUsingPush f) io_purity

{-| Like 'exploreTreeUntilFoundUsingPush' but with a generic impure tree. -}
exploreTreeTUntilFoundUsingPush ::
    (Monoid result, MonadIO m) ⇒
    (result → Bool) {-^ a condition function that signals when we have found all of the result that we wanted -} →
    (∀ α. m α → IO α) {-^ the function that runs the tree's monad in IO -} →
    ThreadsControllerMonad (FoundModeUsingPush result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    TreeT m result {-^ the (impure) tree -} →
    IO (RunOutcome (Progress result) (Either result (Progress result))) {-^ the outcome of the run -}
exploreTreeTUntilFoundUsingPush f run = exploreTreeTUntilFoundUsingPushStartingFrom f run mempty

{-| Like 'exploreTreeTUntilFoundUsingPush', but with a starting progress. -}
exploreTreeTUntilFoundUsingPushStartingFrom ::
    (Monoid result, MonadIO m) ⇒
    (result → Bool) {-^ a condition function that signals when we have found all of the result that we wanted -} →
    (∀ α. m α → IO α) {-^ the function that runs the tree's monad in IO -} →
    Progress result {-^ the starting progress -} →
    ThreadsControllerMonad (FoundModeUsingPush result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    TreeT m result {-^ the (impure) tree -} →
    IO (RunOutcome (Progress result) (Either result (Progress result))) {-^ the outcome of the run -}
exploreTreeTUntilFoundUsingPushStartingFrom f = runExplorer (FoundModeUsingPush f) . ImpureAtopIO

--------------------------------------------------------------------------------
-------------------------------- Generic runner --------------------------------
--------------------------------------------------------------------------------

{-| Explores the given tree using multiple threads to achieve parallelism.

    This function grants access to all of the functionality of this adapter,
    but because its generality complicates its use (primarily the fact that the
    types are dependent on the first parameter) you may find it easier to use
    one of the specialized functions in the preceding section.
 -}
runExplorer ::
    ExplorationMode exploration_mode {-^ the exploration mode -} →
    Purity m n {-^ the purity of the tree -} →
    (ProgressFor exploration_mode) {-^ the starting progress -} →
    ThreadsControllerMonad exploration_mode () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    TreeT m (ResultFor exploration_mode) {-^ the tree -} →
    IO (RunOutcomeFor exploration_mode) {-^ the outcome of the run -}
runExplorer exploration_mode purity starting_progress (C controller) tree =
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

                sendProgressUpdateRequestTo = sendRequestToWorker sendProgressUpdateRequest receiveProgressUpdateFromWorker
                sendWorkloadStealRequestTo = sendRequestToWorker sendWorkloadStealRequest receiveStolenWorkloadFromWorker
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
                            tree
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
