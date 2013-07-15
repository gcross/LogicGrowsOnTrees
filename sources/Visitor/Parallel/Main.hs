{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

{-| This module provides a framework for creating a program that visits a tree
    in parallel. The way that you use it is that you pick the mainFor...
    function that corresponds to the kind of tree generator, and then provide
    the following:

    1. a driver provided by the back-end you want to use;

    2. optional command line arguments that the user can use to specify the tree
       being generated;

    3. a 'TermInfo' value that specifies the description of what this program
       does, a typical example being:

        > defTI { termDoc = "count the number of n-queens solutions for a given board size" }

    4. an action to run when the visit has terminated (a function of the command
       line arguments); and

    5. the tree generator, as a function of the command line arguments.

    Specifically, a program created using this module will automatically take
    care of running the supervisor and the workers (which includes figuring out
    which of the two this program is supposed to run as, where applicable). It
    also provides command line options that specify if, where, and how often a
    checkpoint file should be created (an will resume from an existing
    checkpoint file if it exists) as well as what supervisor statistics (if any)
    should be printed to the screen at the end, and it also provides options for
    the current back-end that specify things like the number of workers to
    create. Your command line options, if any, will be merged in with the rest
    and will be displayed if the user requests help with how to use the program.

    All of this functionality is back-end independent, so if you want to use a
    different back end you only need to change the driver argument.
 -}
module Visitor.Parallel.Main
    (
    -- * Types
    -- ** Driver types
      Driver(..)
    , DriverParameters(..)
    -- ** Outcome types
    , RunOutcome(..)
    , RunOutcomeFor
    , TerminationReason(..)
    , TerminationReasonFor
    -- * Main functions
    -- $main

    -- ** Sum over all results
    -- $all
    , mainForVisitTree
    , mainForVisitTreeIO
    , mainForVisitTreeImpure
    -- ** Stop at first result
    -- $first
    , mainForVisitTreeUntilFirst
    , mainForVisitTreeIOUntilFirst
    , mainForVisitTreeImpureUntilFirst
    -- ** Stop when sum of results found
    -- $found

    -- *** Pull
    -- $pull
    , mainForVisitTreeUntilFoundUsingPull
    , mainForVisitTreeIOUntilFoundUsingPull
    , mainForVisitTreeImpureUntilFoundUsingPull
    -- *** Push
    -- $push
    , mainForVisitTreeUntilFoundUsingPush
    , mainForVisitTreeIOUntilFoundUsingPush
    , mainForVisitTreeImpureUntilFoundUsingPush
    -- ** Generic main function
    , genericMain
    -- * Utility functions
    , extractRunOutcomeFromSupervisorOutcome
    , mainParser
    ) where

import Prelude hiding (readFile,writeFile)

import Control.Applicative ((<$>),(<*>))
import Control.Concurrent (ThreadId,killThread,threadDelay)
import Control.Exception (finally,handleJust,onException)
import Control.Monad (forever,liftM,mplus,when)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Tools (ifM)

import Data.ByteString.Lazy (readFile,writeFile)
import Data.Char (toLower)
import Data.Composition ((.*))
import Data.Derive.Serialize
import Data.DeriveTH
import Data.Functor.Identity (Identity)
import Data.Maybe (catMaybes)
import Data.Monoid (Monoid(..))
import Data.Prefix.Units (FormatMode(FormatSiAll),formatValue,unitName)
import Data.Serialize

import System.Console.CmdTheLine
import System.Directory (doesFileExist,removeFile,renameFile)
import System.Environment (getProgName)
import System.IO (hPutStr,hPutStrLn,stderr)
import System.IO.Error (isDoesNotExistError)
import qualified System.Log.Logger as Logger
import System.Log.Logger (Priority(..),setLevel,rootLoggerName,updateGlobalLogger)
import System.Log.Logger.TH

import Text.Printf (printf)

import Visitor (TreeGenerator,TreeGeneratorIO,TreeGeneratorT)
import Visitor.Checkpoint
import Visitor.Parallel.Common.ExplorationMode
import Visitor.Parallel.Common.Supervisor
    ( FunctionOfTimeStatistics(..)
    , IndependentMeasurementsStatistics(..)
    , RunStatistics(..)
    , SupervisorTerminationReason(..)
    , SupervisorOutcome(..)
    )

import Visitor.Parallel.Common.Supervisor.RequestQueue
import Visitor.Parallel.Common.Worker

--------------------------------------------------------------------------------
----------------------------------- Loggers ------------------------------------
--------------------------------------------------------------------------------

deriveLoggers "Logger" [INFO,NOTICE]

--------------------------------------------------------------------------------
------------------------------------ Types -------------------------------------
--------------------------------------------------------------------------------

-------------------------------- Configuration ---------------------------------

data CheckpointConfiguration = CheckpointConfiguration
    {   checkpoint_path :: FilePath
    ,   checkpoint_interval :: Float
    } deriving (Eq,Show)

data LoggingConfiguration = LoggingConfiguration
    {   log_level :: Priority
    } deriving (Eq,Show)
instance Serialize LoggingConfiguration where
    put = put . show . log_level
    get = LoggingConfiguration . read <$> get

data StatisticsConfiguration = StatisticsConfiguration
    {   show_wall_times :: !Bool
    ,   show_supervisor_occupation :: !Bool
    ,   show_supervisor_monad_occupation :: !Bool
    ,   show_supervisor_calls :: !Bool
    ,   show_worker_occupation :: !Bool
    ,   show_worker_wait_times :: !Bool
    ,   show_steal_wait_times :: !Bool
    ,   show_numbers_of_waiting_workers :: !Bool
    ,   show_numbers_of_available_workloads :: !Bool
    ,   show_instantaneous_workload_request_rates :: !Bool
    ,   show_instantaneous_workload_steal_times :: !Bool
    ,   show_buffer_size :: !Bool
    } deriving (Eq,Show)

data SupervisorConfiguration = SupervisorConfiguration
    {   maybe_checkpoint_configuration :: Maybe CheckpointConfiguration
    ,   statistics_configuration :: StatisticsConfiguration
    } deriving (Eq,Show)

data SharedConfiguration tree_generator_configuration = SharedConfiguration
    {   logging_configuration :: LoggingConfiguration
    ,   tree_generator_configuration :: tree_generator_configuration
    } deriving (Eq,Show)
$( derive makeSerialize ''SharedConfiguration )

--------------------------------- Driver types ---------------------------------

{-| The 'Driver' is the core type that abstracts the various back-ends behind a
    common interface that can be invoked by the main functions; it specifies a
    function that is called to start the run with a set of parameters specified
    in 'DriverParameters'.

    Note that the manager_monad type parameter is within an existential type;
    this is because the user of the driver should not need to know what it is.
 -}
data Driver
    result_monad
    shared_configuration
    supervisor_configuration
    m n
    exploration_mode
  = ∀ manager_monad.
    ( RequestQueueMonad (manager_monad exploration_mode)
    , ExplorationModeFor (manager_monad exploration_mode) ~ exploration_mode
    ) ⇒
    Driver (
        ( Serialize (ProgressFor exploration_mode)
        , MonadIO result_monad
        ) ⇒
        DriverParameters
            shared_configuration
            supervisor_configuration
            m n
            exploration_mode
            manager_monad
        → result_monad ()
    )

{-| The 'DriverParameters' type specifies the information that is given to the
    driver in the main functions.
 -}
data DriverParameters
    shared_configuration
    supervisor_configuration
    m n
    exploration_mode
    manager_monad =
    DriverParameters
    {   {-| the mode in which the visitor is being run -}
        constructExplorationMode :: shared_configuration → ExplorationMode exploration_mode
        {-| the purity of the tree generator -}
    ,   purity :: Purity m n
        {-| configuration options that are shared between the supervisor and the worker -}
    ,   shared_configuration_term :: Term shared_configuration
        {-| configuration options specific to the supervisor -}
    ,   supervisor_configuration_term :: Term supervisor_configuration
        {-| program information;  should at a minimum put a brief description of the program in 'termDoc' -}
    ,   program_info :: TermInfo
        {-| action that initializes the global state of all processes, both worker and supervisor -}
    ,   initializeGlobalState :: shared_configuration → IO ()
        {-| constructs the tree generator given the shared configuration -}
    ,   constructTreeGenerator :: shared_configuration → TreeGeneratorT m (ResultFor exploration_mode)
        {-| in the supervisor process, gets the starting progress for the visit;  this is where a checkpoint is loaded, if one exists -}
    ,   getStartingProgress :: shared_configuration → supervisor_configuration → IO (ProgressFor exploration_mode)
        {-| in the supervisor process, respond to the termination of the run -}
    ,   notifyTerminated :: shared_configuration → supervisor_configuration → RunOutcomeFor exploration_mode → IO ()
        {-| in the supervisor process, construct the manager that does things like periodic checkpointing -}
    ,   constructManager :: shared_configuration → supervisor_configuration → manager_monad exploration_mode ()
    }

-------------------------------- Outcome types ---------------------------------

{-| A type that represents the outcome of a run. -}
data RunOutcome progress final_result = RunOutcome
    {   {-| statistics gathered during the run, useful if the system is not scaling with the number of workers as it should -}
        runStatistics :: RunStatistics
        {-| the reason why the run terminated -}
    ,   runTerminationReason :: TerminationReason progress final_result
    } deriving (Eq,Show)

{-| A convenient type alias that obtains the 'RunOutcome' type for the given exploration mode. -}
type RunOutcomeFor exploration_mode = RunOutcome (ProgressFor exploration_mode) (FinalResultFor exploration_mode)

{-| A type that represents the reason why a run terminated. -}
data TerminationReason progress final_result =
    {-| the run was aborted with the given progress -}
    Aborted progress
    {-| the run completed with the given final result -}
  | Completed final_result
    {-| the run failed for the given reason -}
  | Failure String
  deriving (Eq,Show)

{-| A convenient type alias that obtains the 'TerminationReason' type for the given exploration mode. -}
type TerminationReasonFor exploration_mode = TerminationReason (ProgressFor exploration_mode) (FinalResultFor exploration_mode)

--------------------------------------------------------------------------------
---------------------------------- Instances -----------------------------------
--------------------------------------------------------------------------------

instance ArgVal Priority where
    converter = enum $
        [DEBUG,INFO,NOTICE,WARNING,ERROR,CRITICAL,ALERT,EMERGENCY]
        >>=
        \level → let name = show level
                 in return (name,level) `mplus` return (map toLower name,level)

--------------------------------------------------------------------------------
-------------------------------- Main functions ---------------------------------
--------------------------------------------------------------------------------

{- $main
The functions in this section all provide a main function that starts up the
system that visits a tree in parallel using the given tree generator
(constructed possibly using information supplied on the command line) and the
given back-end provided via the driver argument.

All of the functionaliy of this module can be accessed through 'genericMain',
but we nonethless also provide specialized versions of these functions for all
of the supported tree generator purities and exploration modes. This is done for two
reasons: first, in order to make the types more concrete to hopefully improve
usability, and second, because often the type of the tree generator is generic
and so using a specialized function automatically specializes the type rather
than requiring type annotation. The convention is @mainForVisitTreeXY@ where @X@
is empty for pure generators, @IO@ for generators running in the IO monad, and
@Impure@ for generators running in some general monad, and @Y@ specifies the
exploration mode, which is empty for 'AllMode' (sum over all results), @UntilFirst@
for 'FirstMode' (stop when first result found), @UntilFoundUsingPull@ for
'FoundModeUsingPull' (sum all results until a condition has been met, only
sending results to the supervisor upon request) and @UntilFoundUsingPush@ for
'FoundModeUsingPush' (sum all results until a condition has been met, pushing
all found results immediately to the supervisor).
 -}
 
---------------------------- Sum over all results ------------------------------

{- $all #all#
The functions in this section are for when you want to sum over all the results
in (the leaves of) the tree.
 -}

{-| Visit the given tree in parallel using the given pure generator; the results
    in the leaves will be summed up using the 'Monoid' instance.
 -}
mainForVisitTree ::
    (Monoid result, Serialize result, MonadIO result_monad) ⇒
    Driver result_monad (SharedConfiguration tree_generator_configuration) SupervisorConfiguration Identity IO (AllMode result) {-^ the driver for the desired back-end (note that all drivers can be specialized to this type) -} →
    Term tree_generator_configuration {-^ a term with any configuration information needed to construct the tree generator -} →
    TermInfo
        {-^ information about the program; should look something like the following:

                > defTI { termDoc = "count the number of n-queens solutions for a given board size" }
         -} →
    (tree_generator_configuration → RunOutcome (Progress result) result → IO ())
        {-^ a callback that will be invoked with the outcome of the run (as well
            as the tree generator configuration information);  note that if the
            run was 'Completed' then the checkpoint file will be deleted if this
            function finishes successfully
         -} →
    (tree_generator_configuration → TreeGenerator result) {-^ constructs the tree generator given the tree generator configuration information -} →
    result_monad ()
mainForVisitTree = genericMain (const AllMode) Pure

{-| Visit the given tree in parallel using the given generator in the IO monad;
    the results in the leaves will be summed up using the 'Monoid' instance.
 -}
mainForVisitTreeIO ::
    (Monoid result, Serialize result, MonadIO result_monad) ⇒
    Driver result_monad (SharedConfiguration tree_generator_configuration) SupervisorConfiguration IO IO (AllMode result) {-^ the driver for the desired back-end (note that all drivers can be specialized to this type) -} →
    Term tree_generator_configuration {-^ a term with any configuration information needed to construct the tree generator -} →
    TermInfo
        {-^ information about the program; should look something like the following:

                > defTI { termDoc = "count the number of n-queens solutions for a given board size" }
         -} →
    (tree_generator_configuration → RunOutcome (Progress result) result → IO ())
        {-^ a callback that will be invoked with the outcome of the run (as well
            as the tree generator configuration information);  note that if the
            run was 'Completed' then the checkpoint file will be deleted if this
            function finishes successfully
         -} →
    (tree_generator_configuration → TreeGeneratorIO result) {-^ constructs the tree generator given the tree generator configuration information -} →
    result_monad ()
mainForVisitTreeIO = genericMain (const AllMode) io_purity

{-| Visit the given tree in parallel using the given impure generator; the
    results in all of the leaves will be summed up using the 'Monoid' instance.
 -}
mainForVisitTreeImpure ::
    (Monoid result, Serialize result, MonadIO result_monad, Functor m, MonadIO m) ⇒
    (∀ β. m β → IO β) →
    Driver result_monad (SharedConfiguration tree_generator_configuration) SupervisorConfiguration m m (AllMode result) {-^ the driver for the desired back-end (note that all drivers can be specialized to this type) -} →
    Term tree_generator_configuration {-^ a term with any configuration information needed to construct the tree generator -} →
    TermInfo
        {-^ information about the program; should look something like the following:

                > defTI { termDoc = "count the number of n-queens solutions for a given board size" }
         -} →
    (tree_generator_configuration → RunOutcome (Progress result) result → IO ())
        {-^ a callback that will be invoked with the outcome of the run (as well
            as the tree generator configuration information);  note that if the
            run was 'Completed' then the checkpoint file will be deleted if this
            function finishes successfully
         -} →
    (tree_generator_configuration → TreeGeneratorT m result) {-^ constructs the tree generator given the tree generator configuration information -} →
    result_monad ()
mainForVisitTreeImpure = genericMain (const AllMode) . ImpureAtopIO

---------------------------- Stop at first result ------------------------------

{- $first #first#
The functions in this section are for when you want to stop as soon as you have
found a result.

There are two ways in which a system running in this mode can terminate normally:

    1. When a solution is found, in which case a 'Just'-wrapped value is
       returned with both the found solution and the current 'Checkpoint', the
       latter allowing one to resume the search to look for more solutions
       later.

    2. When the whole tree has been visited, in which case 'Nothing' is returned.

 -}

{-| Visit the given tree in parallel using the given pure generator, stopping if
    a solution is found.
 -}
mainForVisitTreeUntilFirst ::
    (Serialize result, MonadIO result_monad) ⇒
    Driver result_monad (SharedConfiguration tree_generator_configuration) SupervisorConfiguration Identity IO (FirstMode result) {-^ the driver for the desired back-end (note that all drivers can be specialized to this type) -} →
    Term tree_generator_configuration {-^ a term with any configuration information needed to construct the tree generator -} →
    TermInfo
        {-^ information about the program; should look something like the following:

                > defTI { termDoc = "count the number of n-queens solutions for a given board size" }
         -} →
    (tree_generator_configuration → RunOutcome Checkpoint (Maybe (Progress result)) → IO ())
        {-^ a callback that will be invoked with the outcome of the run (as well
            as the tree generator configuration information);  note that if the
            run was 'Completed' then the checkpoint file will be deleted if this
            function finishes successfully
         -} →
    (tree_generator_configuration → TreeGenerator result) {-^ constructs the tree generator given the tree generator configuration information -} →
    result_monad ()
mainForVisitTreeUntilFirst = genericMain (const FirstMode) Pure

{-| Visit the given tree in parallel using the given generator in the IO monad,
    stopping if a solution is found.
 -}
mainForVisitTreeIOUntilFirst ::
    (Serialize result, MonadIO result_monad) ⇒
    Driver result_monad (SharedConfiguration tree_generator_configuration) SupervisorConfiguration IO IO (FirstMode result) {-^ the driver for the desired back-end (note that all drivers can be specialized to this type) -} →
    Term tree_generator_configuration {-^ a term with any configuration information needed to construct the tree generator -} →
    TermInfo
        {-^ information about the program; should look something like the following:

                > defTI { termDoc = "count the number of n-queens solutions for a given board size" }
         -} →
    (tree_generator_configuration → RunOutcome Checkpoint (Maybe (Progress result)) → IO ())
        {-^ a callback that will be invoked with the outcome of the run (as well
            as the tree generator configuration information);  note that if the
            run was 'Completed' then the checkpoint file will be deleted if this
            function finishes successfully
         -} →
    (tree_generator_configuration → TreeGeneratorIO result) {-^ constructs the tree generator given the tree generator configuration information -} →
    result_monad ()
mainForVisitTreeIOUntilFirst = genericMain (const FirstMode) io_purity

{-| Visit the given tree in parallel using the given impure generator, stopping
    if a solution is found.
 -}
mainForVisitTreeImpureUntilFirst ::
    (Serialize result, MonadIO result_monad, Functor m, MonadIO m) ⇒
    (∀ β. m β → IO β) →
    Driver result_monad (SharedConfiguration tree_generator_configuration) SupervisorConfiguration m m (FirstMode result) {-^ the driver for the desired back-end (note that all drivers can be specialized to this type) -} →
    Term tree_generator_configuration {-^ a term with any configuration information needed to construct the tree generator -} →
    TermInfo
        {-^ information about the program; should look something like the following:

                > defTI { termDoc = "count the number of n-queens solutions for a given board size" }
         -} →
    (tree_generator_configuration → RunOutcome Checkpoint (Maybe (Progress result)) → IO ())
        {-^ a callback that will be invoked with the outcome of the run (as well
            as the tree generator configuration information);  note that if the
            run was 'Completed' then the checkpoint file will be deleted if this
            function finishes successfully
         -} →
    (tree_generator_configuration → TreeGeneratorT m result) {-^ constructs the tree generator given the tree generator configuration information -} →
    result_monad ()
mainForVisitTreeImpureUntilFirst = genericMain (const FirstMode) . ImpureAtopIO

------------------- Stop when sum of results meets condition -------------------

{- $found #found#
The functions in this section are for when you want sum the results as you find
them until the sum matches a condition.  There are two versions of this mode,
based on whether one wants to regularly poll the workers for results or whether
one wants workers to immediately push every result to the supervisor as soon as
it is found.
 -}

{- $pull #pull#
In this mode, partial results are left on the workers until they receive either
a workload steal request or a progress update request. The advantage of this
approach is that it minimizes communication costs as partial results are sent on
an occasional basis rather than as soon as they are found. The downside of this
approach is that one has to poll the workers on a regular basis, and between
polls it might be the case that the sum of all results in the system meets the
condition but this will not be found out until the next poll, which wastes time
equal to the amount of time between polls. If you would rather have the system
immediately terminate as soon as it has found the desired results (at the price
of paying an additional cost as each workload is found in sending it to the
supervisor), then look at the push mode ("Visitor.Parallel.Main#push").

There are three ways in which a system running in this mode can terminate:

    1. A worker can have its local sum meet the condition;  in this case the
       result of the condition function is returned as well as the partial
       result on the supervisor and the current checkpoint, all wrapped in a
       'Right'; the latter values are provided to allow one one to resume the
       search to find more results at a later time.

    2. The supervisor can have its local sum meet the condition;  this is the
       same as 1 but the partial result is 'mempty'.

    3. The tree can be fully visited, in which case the partial sum is returned
       in a 'Left'.

WARNING:  If you use this mode then you need to enable checkpointing when the
          program is run as if you don't then results will very rarely be pulled
          from the workers and gathered together at the supervisor, meaning that
          the system could spend a long time in a state where the condition
          function is met by the sum total of all results in the system but the
          system does not terminate because it does not know this as the results
          are scattered around.
 -}

{-| Visit the given tree in parallel using the given pure generator until the
    sum of results meets the given condition.
 -}
mainForVisitTreeUntilFoundUsingPull ::
    (Monoid result, Serialize result, MonadIO result_monad) ⇒
    (tree_generator_configuration → result → Maybe final_result) {-^ a condition function that signals when we have found all of the result that we wanted -} →
    Driver result_monad (SharedConfiguration tree_generator_configuration) SupervisorConfiguration Identity IO (FoundModeUsingPull result final_result) {-^ the driver for the desired back-end (note that all drivers can be specialized to this type) -} →
    Term tree_generator_configuration {-^ a term with any configuration information needed to construct the tree generator -} →
    TermInfo
        {-^ information about the program; should look something like the following:

                > defTI { termDoc = "count the number of n-queens solutions for a given board size" }
         -} →
    (tree_generator_configuration → RunOutcome (Progress result) (Either result (Progress (final_result,result))) → IO ())
        {-^ a callback that will be invoked with the outcome of the run (as well
            as the tree generator configuration information);  note that if the
            run was 'Completed' then the checkpoint file will be deleted if this
            function finishes successfully
         -} →
    (tree_generator_configuration → TreeGenerator result) {-^ constructs the tree generator given the tree generator configuration information -} →
    result_monad ()
mainForVisitTreeUntilFoundUsingPull constructCondition = genericMain (FoundModeUsingPull . constructCondition) Pure

{-| Visit the given tree in parallel using the given generator in the IO monad
    until the sum of results meets the given condition.
 -}
mainForVisitTreeIOUntilFoundUsingPull ::
    (Monoid result, Serialize result, MonadIO result_monad) ⇒
    (tree_generator_configuration → result → Maybe final_result) {-^ a condition function that signals when we have found all of the result that we wanted -} →
    Driver result_monad (SharedConfiguration tree_generator_configuration) SupervisorConfiguration IO IO (FoundModeUsingPull result final_result) {-^ the driver for the desired back-end (note that all drivers can be specialized to this type) -} →
    Term tree_generator_configuration {-^ a term with any configuration information needed to construct the tree generator -} →
    TermInfo
        {-^ information about the program; should look something like the following:

                > defTI { termDoc = "count the number of n-queens solutions for a given board size" }
         -} →
    (tree_generator_configuration → RunOutcome (Progress result) (Either result (Progress (final_result,result))) → IO ())
        {-^ a callback that will be invoked with the outcome of the run (as well
            as the tree generator configuration information);  note that if the
            run was 'Completed' then the checkpoint file will be deleted if this
            function finishes successfully
         -} →
    (tree_generator_configuration → TreeGeneratorIO result) {-^ constructs the tree generator given the tree generator configuration information -} →
    result_monad ()
mainForVisitTreeIOUntilFoundUsingPull constructCondition = genericMain (FoundModeUsingPull . constructCondition) io_purity

{-| Visit the given tree in parallel using the given impure generator until the
    sum of results meets the given condition.
 -}
mainForVisitTreeImpureUntilFoundUsingPull ::
    (Monoid result, Serialize result, MonadIO result_monad, Functor m, MonadIO m) ⇒
    (tree_generator_configuration → result → Maybe final_result) {-^ a condition function that signals when we have found all of the result that we wanted -} →
    (∀ β. m β → IO β) →
    Driver result_monad (SharedConfiguration tree_generator_configuration) SupervisorConfiguration m m (FoundModeUsingPull result final_result) {-^ the driver for the desired back-end (note that all drivers can be specialized to this type) -} →
    Term tree_generator_configuration {-^ a term with any configuration information needed to construct the tree generator -} →
    TermInfo
        {-^ information about the program; should look something like the following:

                > defTI { termDoc = "count the number of n-queens solutions for a given board size" }
         -} →
    (tree_generator_configuration → RunOutcome (Progress result) (Either result (Progress (final_result,result))) → IO ())
        {-^ a callback that will be invoked with the outcome of the run (as well
            as the tree generator configuration information);  note that if the
            run was 'Completed' then the checkpoint file will be deleted if this
            function finishes successfully
         -} →
    (tree_generator_configuration → TreeGeneratorT m result) {-^ constructs the tree generator given the tree generator configuration information -} →
    result_monad ()
mainForVisitTreeImpureUntilFoundUsingPull constructCondition = genericMain (FoundModeUsingPull . constructCondition) . ImpureAtopIO

{- $push #push#
In this mode, whenever a result is found it is immediately sent to the
supervisor. The advantage of this approach is that the system finds out
immediately when all the results found so far have met the condition, rather
than waiting for a poll to occur that gathers them together. The downside of
this approach is that it costs some time for a worker to send a result to the
supervisor, so if the condition will not be met until a large number of results
have been found then it be better let the workers accumulate results locally and
to poll them on a regular basis; to do this, see the pull mode
("Visitor.Parallel.Main#pull").

There are three ways in which a system running in this mode can terminate:

    1. The supervisor, have just received a new result from a worker, finds that
       its current sum meets the condition function, in which case it returns
       the result of the condition function plus the current checkpoint (to
       allow the search to be resumed later to find more results) wrapped in a
       'Right'.

    2. The tree can be fully visited, in which case the partial sum is returned
       in a 'Left'.

(Note that, unlike the pull version, a partial result will not be returned upon
success as the Supervisor has access to all results and so it will never be in
the position of only having a partial result upon success.)
 -}

{-| Visit the given tree in parallel using the given pure generator until the
    sum of results meets the given condition.
 -}
mainForVisitTreeUntilFoundUsingPush ::
    (Monoid result, Serialize result, MonadIO result_monad) ⇒
    (tree_generator_configuration → result → Maybe final_result) {-^ a condition function that signals when we have found all of the result that we wanted -} →
    Driver result_monad (SharedConfiguration tree_generator_configuration) SupervisorConfiguration Identity IO (FoundModeUsingPush result final_result) {-^ the driver for the desired back-end (note that all drivers can be specialized to this type) -} →
    Term tree_generator_configuration {-^ a term with any configuration information needed to construct the tree generator -} →
    TermInfo
        {-^ information about the program; should look something like the following:

                > defTI { termDoc = "count the number of n-queens solutions for a given board size" }
         -} →
    (tree_generator_configuration → RunOutcome (Progress result) (Either result (Progress final_result)) → IO ())
        {-^ a callback that will be invoked with the outcome of the run (as well
            as the tree generator configuration information);  note that if the
            run was 'Completed' then the checkpoint file will be deleted if this
            function finishes successfully
         -} →
    (tree_generator_configuration → TreeGenerator result) {-^ constructs the tree generator given the tree generator configuration information -} →
    result_monad ()
mainForVisitTreeUntilFoundUsingPush constructCondition = genericMain (FoundModeUsingPush . constructCondition) Pure

{-| Visit the given tree in parallel using the given generator in the IO monad
    until the sum of results meets the given condition.
 -}
mainForVisitTreeIOUntilFoundUsingPush ::
    (Monoid result, Serialize result, MonadIO result_monad) ⇒
    (tree_generator_configuration → result → Maybe final_result) {-^ a condition function that signals when we have found all of the result that we wanted -} →
    Driver result_monad (SharedConfiguration tree_generator_configuration) SupervisorConfiguration IO IO (FoundModeUsingPush result final_result) {-^ the driver for the desired back-end (note that all drivers can be specialized to this type) -} →
    Term tree_generator_configuration {-^ a term with any configuration information needed to construct the tree generator -} →
    TermInfo
        {-^ information about the program; should look something like the following:

                > defTI { termDoc = "count the number of n-queens solutions for a given board size" }
         -} →
    (tree_generator_configuration → RunOutcome (Progress result) (Either result (Progress final_result)) → IO ())
        {-^ a callback that will be invoked with the outcome of the run (as well
            as the tree generator configuration information);  note that if the
            run was 'Completed' then the checkpoint file will be deleted if this
            function finishes successfully
         -} →
    (tree_generator_configuration → TreeGeneratorIO result) {-^ constructs the tree generator given the tree generator configuration information -} →
    result_monad ()
mainForVisitTreeIOUntilFoundUsingPush constructCondition = genericMain (FoundModeUsingPush . constructCondition)io_purity

{-| Visit the given tree in parallel using the given impure generator until the
    sum of results meets the given condition.
 -}
mainForVisitTreeImpureUntilFoundUsingPush ::
    (Monoid result, Serialize result, MonadIO result_monad, Functor m, MonadIO m) ⇒
    (tree_generator_configuration → result → Maybe final_result) {-^ a condition function that signals when we have found all of the result that we wanted -} →
    (∀ β. m β → IO β) →
    Driver result_monad (SharedConfiguration tree_generator_configuration) SupervisorConfiguration m m (FoundModeUsingPush result final_result) {-^ the driver for the desired back-end (note that all drivers can be specialized to this type) -} →
    Term tree_generator_configuration {-^ a term with any configuration information needed to construct the tree generator -} →
    TermInfo
        {-^ information about the program; should look something like the following:

                > defTI { termDoc = "count the number of n-queens solutions for a given board size" }
         -} →
    (tree_generator_configuration → RunOutcome (Progress result) (Either result (Progress final_result)) → IO ())
        {-^ a callback that will be invoked with the outcome of the run (as well
            as the tree generator configuration information);  note that if the
            run was 'Completed' then the checkpoint file will be deleted if this
            function finishes successfully
         -} →
    (tree_generator_configuration → TreeGeneratorT m result) {-^ constructs the tree generator given the tree generator configuration information -} →
    result_monad ()
mainForVisitTreeImpureUntilFoundUsingPush constructCondition = genericMain (FoundModeUsingPush . constructCondition) . ImpureAtopIO

---------------------------- Generic main function -----------------------------

{-| This function is just like those in the previous functions except that it is
    generalized over all tree generator purities and exploration modes.
 -}
genericMain ::
    ( MonadIO result_monad
    , ResultFor exploration_mode ~ result
    , Serialize (ProgressFor exploration_mode)
    ) ⇒
    (tree_generator_configuration → ExplorationMode exploration_mode)
        {-^ constructs the exploration mode given the tree generator configuration;
            note that the constructor that this function returns is restricted
            by the value of the exploration_mode type variable
         -} →
    Purity m n {-^ the purity of the tree generator -} →
    Driver
        result_monad
        (SharedConfiguration tree_generator_configuration)
        SupervisorConfiguration
        m n
        exploration_mode
        {-^ the driver for the desired back-end (note that all drivers can be specialized to this type) -} →
    Term tree_generator_configuration {-^ a term with any configuration information needed to construct the tree generator -} →
    TermInfo
        {-^ information about the program; should look something like the following:

                > defTI { termDoc = "count the number of n-queens solutions for a given board size" }
         -} →
    (tree_generator_configuration → RunOutcomeFor exploration_mode → IO ())
        {-^ a callback that will be invoked with the outcome of the run (as well
            as the tree generator configuration information);  note that if the
            run was 'Completed' then the checkpoint file will be deleted if this
            function finishes successfully
         -} →
    (tree_generator_configuration → TreeGeneratorT m result) {-^ constructs the tree generator given the tree generator configuration information -} →
    result_monad ()
genericMain constructExplorationMode_ purity (Driver run) tree_generator_configuration_term program_info notifyTerminated_ constructTreeGenerator_ =
    run DriverParameters{..}
  where
    constructExplorationMode = constructExplorationMode_ . tree_generator_configuration
    shared_configuration_term = makeSharedConfigurationTerm tree_generator_configuration_term
    supervisor_configuration_term =
        SupervisorConfiguration
            <$> checkpoint_configuration_term
            <*> statistics_configuration_term
    initializeGlobalState SharedConfiguration{logging_configuration=LoggingConfiguration{..}} =
        updateGlobalLogger rootLoggerName (setLevel log_level)
    constructTreeGenerator = constructTreeGenerator_ . tree_generator_configuration
    getStartingProgress shared_configuration SupervisorConfiguration{..} =
        case maybe_checkpoint_configuration of
            Nothing → (infoM "Checkpointing is NOT enabled") >> return initial_progress
            Just CheckpointConfiguration{..} → do
                noticeM $ "Checkpointing enabled"
                noticeM $ "Checkpoint file is " ++ checkpoint_path
                noticeM $ "Checkpoint interval is " ++ show checkpoint_interval ++ " seconds"
                ifM (doesFileExist checkpoint_path)
                    (noticeM "Loading existing checkpoint file" >> either error id . decodeLazy <$> readFile checkpoint_path)
                    (return initial_progress)
      where
        initial_progress = initialProgress . constructExplorationMode $ shared_configuration
    notifyTerminated SharedConfiguration{..} SupervisorConfiguration{..} run_outcome@RunOutcome{..} =
        do showStatistics statistics_configuration runStatistics
           notifyTerminated_ tree_generator_configuration run_outcome
        `finally`
        case maybe_checkpoint_configuration of
            Nothing → return ()
            Just CheckpointConfiguration{checkpoint_path} →
                let deleteCheckpointFile = do
                        noticeM "Deleting any remaining checkpoint file"
                        removeFileIfExists checkpoint_path
                in case runTerminationReason of
                    Aborted checkpoint → writeCheckpointFile checkpoint_path checkpoint
                    Completed _ → deleteCheckpointFile
                    Failure _ → deleteCheckpointFile
    constructManager = const managerLoop

------------------------------ Utility functions -------------------------------

{-| Converts a 'SupervisorOutcome' to a 'RunOutcome'. -}
extractRunOutcomeFromSupervisorOutcome ::
    Show worker_id ⇒
    SupervisorOutcome fv ip worker_id →
    RunOutcome ip fv
extractRunOutcomeFromSupervisorOutcome SupervisorOutcome{..} = RunOutcome{..}
  where
    runTerminationReason =
        case supervisorTerminationReason of
            SupervisorAborted remaining_progress → Aborted remaining_progress
            SupervisorCompleted result → Completed result
            SupervisorFailure worker_id message →
                Failure $ "Worker " ++ show worker_id ++ " failed with message: " ++ message
    runStatistics = supervisorRunStatistics

{-| Parse the command line options using the given term and term info (the
    latter of which has the program name added to it);  if successful return the
    result, otherwise throw an exception.
 -}
mainParser :: Term α → TermInfo → IO α
mainParser term term_info =
    (if null (termName term_info)
        then getProgName >>= \progname → return $ term_info {termName = progname}
        else return term_info
    ) >>= exec . (term,)

--------------------------------------------------------------------------------
----------------------------------- Internal -----------------------------------
--------------------------------------------------------------------------------

checkpoint_configuration_term :: Term (Maybe CheckpointConfiguration)
checkpoint_configuration_term =
    maybe (const Nothing) (Just .* CheckpointConfiguration)
        <$> value (flip opt (
            (optInfo ["c","checkpoint-file"])
            {   optName = "FILEPATH"
            ,   optDoc = "This enables periodic checkpointing with the given path specifying the location of the checkpoint file;  if the file already exists then it will be loaded as the initial starting point for the search."
            }
            ) Nothing)
        <*> value (flip opt (
            (optInfo ["i","checkpoint-interval"])
            {   optName = "SECONDS"
            ,   optDoc = "This specifies the time between checkpoints (in seconds, decimals allowed); it is ignored if checkpoint file is not specified."
            }
            ) 60)

logging_configuration_term :: Term LoggingConfiguration
logging_configuration_term =
    LoggingConfiguration
    <$> value (flip opt (
        (optInfo ["l","log-level"])
        {   optName = "LEVEL"
        ,   optDoc = "This specifies the upper bound (inclusive) on the importance of the messages that will be logged;  it must be one of (in increasing order of importance): DEBUG, INFO, NOTICE, WARNING, ERROR, CRITICAL, ALERT, or EMERGENCY."
        }
        ) WARNING)

statistics_configuration_term :: Term StatisticsConfiguration
statistics_configuration_term =
    (\show_all → if show_all then const (StatisticsConfiguration True True True True True True True True True True True True) else id)
    <$> value (flag ((optInfo ["show-all"]) { optDoc ="This option will cause *all* run statistic to be printed to standard error after the program terminates." }))
    <*> (StatisticsConfiguration
        <$> value (flag ((optInfo ["show-walltimes"]) { optDoc ="This option will cause the starting, ending, and duration wall time of the run to be printed to standard error after the program terminates." }))
        <*> value (flag ((optInfo ["show-supervisor-occupation"]) { optDoc ="This option will cause the supervisor occupation percentage to be printed to standard error after the program terminates." }))
        <*> value (flag ((optInfo ["show-supervisor-monad-occupation"]) { optDoc ="This option will cause the supervisor monad occupation percentage to be printed to standard error after the program terminates." }))
        <*> value (flag ((optInfo ["show-supervisor-calls"]) { optDoc ="This option will cause the number of supervisor calls and average time per supervisor call to be printed to standard error after the program terminates." }))
        <*> value (flag ((optInfo ["show-worker-occupation"]) { optDoc ="This option will cause the worker occupation percentage to be printed to standard error after the program terminates." }))
        <*> value (flag ((optInfo ["show-worker-wait-times"]) { optDoc ="This option will cause statistics about the worker wait times to be printed to standard error after the program terminates." }))
        <*> value (flag ((optInfo ["show-steal-wait-times"]) { optDoc ="This option will cause statistics about the steal wait times to be printed to standard error after the program terminates." }))
        <*> value (flag ((optInfo ["show-numbers-of-waiting-workers"]) { optDoc ="This option will cause statistics about the number of waiting workers to be printed to standard error after the program terminates." }))
        <*> value (flag ((optInfo ["show-numbers-of-available-workloads"]) { optDoc ="This option will cause statistics about the number of available workloads to be printed to standard error after the program terminates." }))
        <*> value (flag ((optInfo ["show-workload-request-rate"]) { optDoc ="This option will cause statistics about the (roughly) instantaneous rate at which workloads are requested by finished works to be printed to standard error after the program terminates." }))
        <*> value (flag ((optInfo ["show-workload-steal-time"]) { optDoc ="This option will cause statistics about the (roughly) instantaneous amount of time that it took to steal a workload to be printed to standard error after the program terminates." }))
        <*> value (flag ((optInfo ["show-buffer-size"]) { optDoc ="This option will cause statistics about the buffer size to be printed to standard error after the program terminates." }))
        )

makeSharedConfigurationTerm :: Term tree_generator_configuration → Term (SharedConfiguration tree_generator_configuration)
makeSharedConfigurationTerm tree_generator_configuration_term =
    SharedConfiguration
        <$> logging_configuration_term
        <*> tree_generator_configuration_term

checkpointLoop ::
    ( RequestQueueMonad m
    , Serialize (ProgressFor (ExplorationModeFor m))
    ) ⇒ CheckpointConfiguration → m α
checkpointLoop CheckpointConfiguration{..} = forever $ do
    liftIO $ threadDelay delay
    requestProgressUpdate >>= writeCheckpointFile checkpoint_path
  where
    delay = round $ checkpoint_interval * 1000000

managerLoop ::
    ( RequestQueueMonad m
    , Serialize (ProgressFor (ExplorationModeFor m))
    ) ⇒ SupervisorConfiguration → m ()
managerLoop SupervisorConfiguration{..} = do
    maybe_checkpoint_thread_id ← maybeForkIO checkpointLoop maybe_checkpoint_configuration
    case catMaybes
        [maybe_checkpoint_thread_id
        ]
     of [] → return ()
        thread_ids → liftIO $
            (forever $ threadDelay 3600000000)
            `finally`
            (mapM_ killThread thread_ids)

maybeForkIO :: RequestQueueMonad m ⇒ (α → m ()) → Maybe α → m (Maybe ThreadId)
maybeForkIO loop = maybe (return Nothing) (liftM Just . fork . loop)

removeFileIfExists :: FilePath → IO ()
removeFileIfExists path =
    handleJust
        (\e → if isDoesNotExistError e then Nothing else Just ())
        (\_ → return ())
        (removeFile path)

showStatistics :: MonadIO m ⇒ StatisticsConfiguration → RunStatistics → m ()
showStatistics StatisticsConfiguration{..} RunStatistics{..} = liftIO $ do
    let total_time :: Double
        total_time = realToFrac runWallTime
    when show_wall_times $
        hPutStrLn stderr $
            printf "Run started at %s, ended at %s, and took %sseconds.\n"
                (show runStartTime)
                (show runEndTime)
                (showWithUnitPrefix total_time)
    hPutStr stderr $
        case (show_supervisor_occupation,show_supervisor_monad_occupation) of
            (True,False) → printf "Supervior was occupied for %.2f%% of the run.\n\n" (runSupervisorOccupation*100)
            (False,True) → printf "Supervisor ran inside the SupervisorMonad for %.2f%% of the run.\n\n" (runSupervisorMonadOccupation*100)
            (True,True) → printf "Supervior was occupied for %.2f%% of the run, of which %.2f%% was spent inside the SupervisorMonad.\n\n" (runSupervisorOccupation*100) (runSupervisorOccupation/runSupervisorMonadOccupation*100)
            _ → ""
    when show_supervisor_calls $
        hPutStrLn stderr $
            printf "%i calls were made into the supervisor monad, and each took an average of %sseconds.\n"
                runNumberOfCalls
                (showWithUnitPrefix runAverageTimePerCall)
    when show_worker_occupation $
        hPutStrLn stderr $
            printf "Workers were occupied %.2f%% of the time on average.\n"
                (runWorkerOccupation*100)
    when show_worker_wait_times $ do
        let FunctionOfTimeStatistics{..} = runWorkerWaitTimes
        hPutStrLn stderr $
          if timeCount == 0
            then
              "At no point did a worker receive a new workload after finishing a workload."
            else
              if timeMax == 0
                then
                  printf "Workers completed their task and obtained a new workload %i times and never had to wait to receive the new workload."
                    timeCount
                else
                  printf
                    (unlines
                        ["Workers completed their task and obtained a new workload %i times with an average of one every %sseconds or %.1g enqueues/second."
                        ,"The minimum waiting time was %sseconds, and the maximum waiting time was %sseconds."
                        ,"On average, a worker had to wait %sseconds +/- %sseconds (std. dev) for a new workload."
                        ]
                    )
                    timeCount
                    (showWithUnitPrefix $ total_time / fromIntegral timeCount)
                    (fromIntegral timeCount / total_time)
                    (showWithUnitPrefix timeMin)
                    (showWithUnitPrefix timeMax)
                    (showWithUnitPrefix timeAverage)
                    (showWithUnitPrefix timeStdDev)
    when show_steal_wait_times $ do
        let IndependentMeasurementsStatistics{..} = runStealWaitTimes
        hPutStrLn stderr $
          if statCount == 0
            then
              "No workloads were stolen."
            else
              printf
                (unlines
                    ["Workloads were stolen %i times with an average of %sseconds between each steal or %.1g steals/second."
                    ,"The minimum waiting time for a steal was %sseconds, and the maximum waiting time was %sseconds."
                    ,"On average, it took %sseconds +/- %sseconds (std. dev) to steal a workload."
                    ]
                )
                statCount
                (showWithUnitPrefix $ total_time / fromIntegral statCount)
                (fromIntegral statCount / total_time)
                (showWithUnitPrefix statMin)
                (showWithUnitPrefix statMax)
                (showWithUnitPrefix statAverage)
                (showWithUnitPrefix statStdDev)
    when show_numbers_of_waiting_workers $ do
        let FunctionOfTimeStatistics{..} = runWaitingWorkerStatistics
        hPutStrLn stderr $
          if timeMax == 0
            then
              printf "No worker ever had to wait for a workload to become available.\n"
            else if timeMin == 0
              then
                printf "On average, %.1f +/ - %.1f (std. dev) workers were waiting at any given time;  never more than %i.\n"
                  timeAverage
                  timeStdDev
                  timeMax
              else
                printf "On average, %.1f +/ - %.1f (std. dev) workers were waiting at any given time;  never more than %i nor fewer than %i.\n"
                  timeAverage
                  timeStdDev
                  timeMax
                  timeMin
    when show_numbers_of_available_workloads $ do
        let FunctionOfTimeStatistics{..} = runAvailableWorkloadStatistics
        hPutStrLn stderr $
          if timeMax == 0
            then
              printf "No workload ever had to wait for an available worker.\n"
            else if timeMin == 0
              then
                printf "On average, %.1f +/ - %.1f (std. dev) workloads were waiting for a worker at any given time;  never more than %i.\n"
                  timeAverage
                  timeStdDev
                  timeMax
              else
                printf "On average, %.1f +/ - %.1f (std. dev) workloads were waiting for a worker at any given time;  never more than %i nor fewer than %i.\n"
                  timeAverage
                  timeStdDev
                  timeMax
                  timeMin
    when show_instantaneous_workload_request_rates $ do
        let FunctionOfTimeStatistics{..} = runInstantaneousWorkloadRequestRateStatistics
        hPutStrLn stderr $
            printf
                (unlines
                    ["On average, the instantanenous rate at which workloads were being requested was %.1f +/ - %.1f (std. dev) requests per second;  the rate never fell below %.1f nor rose above %.1f."
                    ,"This value was obtained by exponentially smoothing the request data over a time scale of one second."
                    ]
                )
                timeAverage
                timeStdDev
                timeMin
                timeMax
    when show_instantaneous_workload_steal_times $ do
        let FunctionOfTimeStatistics{..} = runInstantaneousWorkloadStealTimeStatistics
        hPutStrLn stderr $
            printf
                (unlines
                    ["On average, the instantaneous time to steal a workload was %sseconds +/ - %sseconds (std. dev);  this time interval never fell below %sseconds nor rose above %sseconds."
                    ,"This value was obtained by exponentially smoothing the request data over a time scale of one second."
                    ]
                )
                (showWithUnitPrefix timeAverage)
                (showWithUnitPrefix timeStdDev)
                (showWithUnitPrefix timeMin)
                (showWithUnitPrefix timeMax)
    when show_buffer_size $ do
        let FunctionOfTimeStatistics{..} = runBufferSizeStatistics
        hPutStrLn stderr $
            printf "On average, the buffer size was %.1f +/ - %.1f (std. dev);  it was never smaller than %i, nor greater than %i.\n"
                timeAverage
                timeStdDev
                timeMin
                timeMax
  where
    showWithUnitPrefix :: Real n ⇒ n → String
    showWithUnitPrefix 0 = "0 "
    showWithUnitPrefix x = printf "%.1f %s" x_scaled (unitName unit)
      where
        (x_scaled :: Float,Just unit) = formatValue (Left FormatSiAll) . realToFrac $ x

writeCheckpointFile :: (Serialize ip, MonadIO m) ⇒ FilePath → ip → m ()
writeCheckpointFile checkpoint_path checkpoint = do
    noticeM $ "Writing checkpoint file"
    liftIO $
        (do writeFile checkpoint_temp_path (encodeLazy checkpoint)
            renameFile checkpoint_temp_path checkpoint_path
        ) `onException` (
            removeFileIfExists checkpoint_temp_path
        )
  where
    checkpoint_temp_path = checkpoint_path ++ ".tmp"
