{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

{-| This adapter implements parallelism by spawning multiple worker threads, the
    number of which can be changed arbitrarily during the run.

    NOTE: For the use of threads to results in parallelization, you need to make
    sure that the number of capabilities is at least as large as the largest
    number of worker threads you will be spawning. If you are using the
    'driver', then this will be taken care of for you. If not, then you will
    need to either call 'GHC.Conc.setNumCapabilities' (but only to increase the
    number of threads in GHC 7.4, and not too often as it may crash) or use the
    command-line argument @+RTS -N\#@, where @\#@ is the number of threads you
    want to run in parallel. The 'driver' takes care of this automatically by
    calling 'setNumCapabilities' a single time to set the number of capabilities
    equal to the number of request threads (provided via. a command-line
    argument).
 -}
module LogicGrowsOnTrees.Parallel.Adapter.Threads
    (
    -- * Driver
      driver
    -- * Controller
    , ThreadsControllerMonad
    , abort
    , changeNumberOfWorkersAsync
    , changeNumberOfWorkers
    , changeNumberOfWorkersToMatchCapabilities
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

import Control.Applicative (Applicative,(<$>),(<*>))
import Control.Concurrent (getNumCapabilities,killThread)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Control.Monad.Trans.State.Strict (get,modify)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(mempty))
import Data.Void (absurd)

import GHC.Conc (setNumCapabilities)

import Options.Applicative
    ( auto
    , execParser
    , help
    , info
    , long
    , option
    , short
    , showDefault
    , value
    )

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

{-| This is the driver for the threads adapter.  The number of workers is
    specified via. the command-line option "-n"; 'setNumCapabilities' is called
    exactly once to make sure that there is an equal number of capabilities.
 -}
driver :: Driver IO shared_configuration supervisor_configuration m n exploration_mode
driver = Driver $ \DriverParameters{..} → do
    (tree_configuration,supervisor_configuration,number_of_threads) ← execParser $
        info
            ((,,) <$> tree_configuration_parser
                  <*> supervisor_configuration_parser
                  <*> number_of_threads_parser
            )
            program_info
    initializeGlobalState supervisor_configuration
    starting_progress ← getStartingProgress tree_configuration supervisor_configuration
    runExplorer
        (constructExplorationMode tree_configuration)
         purity
         starting_progress
        (do liftIO $ do
                number_of_capabilities ← getNumCapabilities
                when (number_of_capabilities < number_of_threads) $
                    setNumCapabilities number_of_threads
            setNumberOfWorkersAsync
                (fromIntegral number_of_threads)
                (return ())
            constructController supervisor_configuration
        )
        (constructTree tree_configuration)
     >>= notifyTerminated tree_configuration supervisor_configuration
  where
    number_of_threads_parser = option auto $ mconcat
        [ short 'n'
        , long "number-of-threads"
        , help "The number of worker threads to spawn."
        , value 1
        , showDefault
        ]
{-# INLINE driver #-}

--------------------------------------------------------------------------------
---------------------------------- Controller ----------------------------------
--------------------------------------------------------------------------------

{-| This is the monad in which the thread controller will run. -}
newtype ThreadsControllerMonad exploration_mode α =
    C (WorkgroupControllerMonad (IntMap (WorkerEnvironment (ProgressFor exploration_mode))) exploration_mode α)
  deriving (Applicative,Functor,Monad,MonadIO,RequestQueueMonad,WorkgroupRequestQueueMonad)

instance HasExplorationMode (ThreadsControllerMonad exploration_mode) where
    type ExplorationModeFor (ThreadsControllerMonad exploration_mode) = exploration_mode

{-| Changes the number of a parallel workers to equal the number of capabilities
    as reported by 'getNumCapabilities'.
 -}
changeNumberOfWorkersToMatchCapabilities :: ThreadsControllerMonad exploration_mode ()
changeNumberOfWorkersToMatchCapabilities =
    liftIO getNumCapabilities >>= flip setNumberOfWorkersAsync (return ()) . fromIntegral

--------------------------------------------------------------------------------
---------------------------- Exploration functions -----------------------------
--------------------------------------------------------------------------------

{- $exploration
The functions in this section are provided as a way to use the Threads adapter
directly rather than using the framework provided in
"LogicGrowsOnTrees.Parallel.Main". They are all specialized versions of
'runExplorer', which appears in the following section. The specialized versions
are provided for convenience --- specifically, to minimize the knowledge needed
of the implementation and how the types specialize for the various exploration
modes.

There are 3 × 2 × 4 = 24 functions in this section; the factor of 3 comes from
the fact that there are three cases of monad in which the exploration is run:
pure, IO, and impure (where IO is a special case of impure provided for
convenience); the factor of 2 comes from the fact that one can either start with
no progress or start with a given progress; and the factor of 4 comes from the
fact that there are four exploration modes: summing over all results, returning
the first result, summing over all results until a criteria is met with
intermediate results only being sent to the supervisor upon request, and the
previous mode but with all intermediate results being sent immediately to the
supervisor.
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
{-# INLINE exploreTree #-}

{-| Like 'exploreTree' but with a starting progress. -}
exploreTreeStartingFrom ::
    Monoid result ⇒
    Progress result {-^ the starting progress -} →
    ThreadsControllerMonad (AllMode result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    Tree result {-^ the (pure) tree -} →
    IO (RunOutcome (Progress result) result) {-^ the outcome of the run -}
exploreTreeStartingFrom = runExplorer AllMode Pure
{-# INLINE exploreTreeStartingFrom #-}

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
    (∀ α. m α → IO α) {-^ a function that runs the tree's monad in IO -} →
    ThreadsControllerMonad (AllMode result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    TreeT m result {-^ the (impure) tree -} →
    IO (RunOutcome (Progress result) result) {-^ the outcome of the run -}
exploreTreeT runner controller tree = exploreTreeTStartingFrom runner mempty controller tree

{-| Like 'exploreTreeT', but with a starting progress. -}
exploreTreeTStartingFrom ::
    (Monoid result, MonadIO m) ⇒
    (∀ α. m α → IO α) {-^ a function that runs the tree's monad in IO -} →
    Progress result {-^ the starting progress -} →
    ThreadsControllerMonad (AllMode result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    TreeT m result {-^ the (impure) tree -} →
    IO (RunOutcome (Progress result) result)
exploreTreeTStartingFrom run = runExplorer AllMode (ImpureAtopIO run)

---------------------------- Stop at first result ------------------------------

{- $first
For more details, follow this link: "LogicGrowsOnTrees.Parallel.Main#first"
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
    (∀ α. m α → IO α) {-^ a function that runs the tree's monad in IO -} →
    ThreadsControllerMonad (FirstMode result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    TreeT m result {-^ the (impure) tree -} →
    IO (RunOutcome Checkpoint (Maybe (Progress result))) {-^ the outcome of the run -}
exploreTreeTUntilFirst runner controller tree = exploreTreeTUntilFirstStartingFrom runner mempty controller tree

{-| Like 'exploreTreeTUntilFirst', but with a starting progress. -}
exploreTreeTUntilFirstStartingFrom ::
    MonadIO m ⇒
    (∀ α. m α → IO α) {-^ a function that runs the tree's monad in IO -} →
    Checkpoint {-^ the starting progress -} →
    ThreadsControllerMonad (FirstMode result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    TreeT m result {-^ the (impure) tree -} →
    IO (RunOutcome Checkpoint (Maybe (Progress result))) {-^ the outcome of the run -}
exploreTreeTUntilFirstStartingFrom run = runExplorer FirstMode (ImpureAtopIO run)

------------------------ Stop when sum of results found ------------------------

{- $pull
For more details, follow this link: "LogicGrowsOnTrees.Parallel.Main#pull"

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
    (∀ α. m α → IO α) {-^ a function that runs the tree's monad in IO -} →
    ThreadsControllerMonad (FoundModeUsingPull result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    TreeT m result {-^ the (impure) tree -} →
    IO (RunOutcome (Progress result) (Either result (Progress result))) {-^ the outcome of the run -}
exploreTreeTUntilFoundUsingPull f run = exploreTreeTUntilFoundUsingPullStartingFrom f run mempty

{-| Like 'exploreTreeTUntilFoundUsingPull' but with a starting progress. -}
exploreTreeTUntilFoundUsingPullStartingFrom ::
    (Monoid result, MonadIO m) ⇒
    (result → Bool) {-^ a condition function that signals when we have found all of the result that we wanted -} →
    (∀ α. m α → IO α) {-^ a function that runs the tree's monad in IO -} →
    Progress result {-^ the starting progress -} →
    ThreadsControllerMonad (FoundModeUsingPull result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    TreeT m result {-^ the (impure) tree -} →
    IO (RunOutcome (Progress result) (Either result (Progress result))) {-^ the outcome of the run -}
exploreTreeTUntilFoundUsingPullStartingFrom f run = runExplorer (FoundModeUsingPull f) (ImpureAtopIO run)

{- $push
For more details, follow this link: "LogicGrowsOnTrees.Parallel.Main#push"
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
    (∀ α. m α → IO α) {-^ a function that runs the tree's monad in IO -} →
    ThreadsControllerMonad (FoundModeUsingPush result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    TreeT m result {-^ the (impure) tree -} →
    IO (RunOutcome (Progress result) (Either result (Progress result))) {-^ the outcome of the run -}
exploreTreeTUntilFoundUsingPush f run = exploreTreeTUntilFoundUsingPushStartingFrom f run mempty

{-| Like 'exploreTreeTUntilFoundUsingPush', but with a starting progress. -}
exploreTreeTUntilFoundUsingPushStartingFrom ::
    (Monoid result, MonadIO m) ⇒
    (result → Bool) {-^ a condition function that signals when we have found all of the result that we wanted -} →
    (∀ α. m α → IO α) {-^ a function that runs the tree's monad in IO -} →
    Progress result {-^ the starting progress -} →
    ThreadsControllerMonad (FoundModeUsingPush result) () {-^ the controller loop, which at the very least must start by increasing the number of workers from 0 to the desired number -} →
    TreeT m result {-^ the (impure) tree -} →
    IO (RunOutcome (Progress result) (Either result (Progress result))) {-^ the outcome of the run -}
exploreTreeTUntilFoundUsingPushStartingFrom f run = runExplorer (FoundModeUsingPush f) (ImpureAtopIO run)

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
{-# INLINE runExplorer #-}

--------------------------------------------------------------------------------
----------------------------------- Internal -----------------------------------
--------------------------------------------------------------------------------

fromJustOrBust :: String → Maybe α → α
fromJustOrBust message = fromMaybe (error message)
