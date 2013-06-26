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

    1) a driver provided by the back-end you want to use;

    2) optional command line arguments that the user can use to specify the tree
       being generated;

    3) a 'TermInfo' value that specifies the description of what this program
       does, a typical example being:

        > defTI { termDoc = "count the number of n-queens solutions for a given board size" }

    4) an action to run when the visit has terminated (a function of the command line arguments); and

    5) the tree generator, as a function of the command line arguments.

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
    -- ** Specialized driver types
    -- $specialized
    , AllModePureGeneratorDriver
    , AllModeIOGeneratorDriver
    , AllModeImpureGeneratorDriver
    -- ** Outcome types
    , RunOutcome(..)
    , RunOutcomeFor
    , TerminationReason(..)
    , TerminationReasonFor
    -- * Functions
    -- ** Main functions
    -- $main
    , mainForVisitTree
    , mainForVisitTreeIO
    , mainForVisitTreeImpure
    , genericMain
    -- ** Utility functions
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
import Visitor.Parallel.Common.Supervisor
    ( FunctionOfTimeStatistics(..)
    , IndependentMeasurementsStatistics(..)
    , RunStatistics(..)
    , SupervisorTerminationReason(..)
    , SupervisorOutcome(..)
    )

import Visitor.Parallel.Common.Supervisor.RequestQueue
import Visitor.Parallel.Common.VisitorMode
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
    visitor_mode
  = ∀ manager_monad.
    ( RequestQueueMonad (manager_monad visitor_mode)
    , VisitorModeFor (manager_monad visitor_mode) ~ visitor_mode
    ) ⇒
    Driver (
        ( Serialize (ProgressFor visitor_mode)
        , MonadIO result_monad
        ) ⇒
        DriverParameters
            shared_configuration
            supervisor_configuration
            m n
            visitor_mode
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
    visitor_mode
    manager_monad =
    DriverParameters
    {   {-| the mode in which the visitor is being run -}
        visitor_mode :: VisitorMode visitor_mode
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
    ,   constructTreeGenerator :: shared_configuration → TreeGeneratorT m (ResultFor visitor_mode)
        {-| in the supervisor process, gets the starting progress for the visit;  this is where a checkpoint is loaded, if one exists -}
    ,   getStartingProgress :: shared_configuration → supervisor_configuration → IO (ProgressFor visitor_mode)
        {-| in the supervisor process, respond to the termination of the run -}
    ,   notifyTerminated :: shared_configuration → supervisor_configuration → RunOutcomeFor visitor_mode → IO ()
        {-| in the supervisor process, construct the manager that does things like periodic checkpointing -}
    ,   constructManager :: shared_configuration → supervisor_configuration → manager_monad visitor_mode ()
    }

--------------------------- Specialized driver types ---------------------------

{- $specialized
The types in this section are the driver types specialized for the supported
visitor modes and tree generator purities.  They are provided both to slightly
simplify the signatures for the main functions and also to document what driver
types you get when you specialize the visitor more and purity.

Note that all drivers can be specialized to any of these types, so when you are
passing a driver as an argument you can treat it as being whichever of these
types is the type of the parameter.
 -}

{-| The type of drivers with a pure generator that sum over all results. -}
type AllModePureGeneratorDriver tree_generator_configuration result_monad result =
    Driver
        result_monad
        (SharedConfiguration tree_generator_configuration)
        SupervisorConfiguration
        Identity IO
        (AllMode result)

{-| The type of drivers with an IO monad generator that sum over all results. -}
type AllModeIOGeneratorDriver tree_generator_configuration result_monad result =
    Driver
        result_monad
        (SharedConfiguration tree_generator_configuration)
        SupervisorConfiguration
        IO IO
        (AllMode result)

{-| The type of drivers with an impure generator that sum over all results. -}
type AllModeImpureGeneratorDriver tree_generator_configuration result_monad m result =
    Driver
        result_monad
        (SharedConfiguration tree_generator_configuration)
        SupervisorConfiguration
        m m
        (AllMode result)

-------------------------------- Outcome types ---------------------------------

{-| A type that represents the outcome of a run. -}
data RunOutcome progress final_result = RunOutcome
    {   {-| statistics gathered during the run, useful if the system is not scaling with the number of workers as it should -}
        runStatistics :: RunStatistics
        {-| the reason why the run terminated -}
    ,   runTerminationReason :: TerminationReason progress final_result
    } deriving (Eq,Show)

{-| A convenient type alias that obtains the 'RunOutcome' type for the given visitor mode. -}
type RunOutcomeFor visitor_mode = RunOutcome (ProgressFor visitor_mode) (FinalResultFor visitor_mode)

{-| A type that represents the reason why a run terminated. -}
data TerminationReason progress final_result =
    {-| the run was aborted with the given progress -}
    Aborted progress
    {-| the run completed with the given final result -}
  | Completed final_result
    {-| the run failed for the given reason -}
  | Failure String
  deriving (Eq,Show)

{-| A convenient type alias that obtains the 'TerminationReason' type for the given visitor mode. -}
type TerminationReasonFor visitor_mode = TerminationReason (ProgressFor visitor_mode) (FinalResultFor visitor_mode)

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
---------------------------------- Functions -----------------------------------
--------------------------------------------------------------------------------

-------------------------------- Main functions --------------------------------

{- $main
The functions in this section all provide a main function that starts up the
system that visits a tree in parallel using the given tree generator
(constructed possibly using information supplied on the command line) and the
given back-end provided via the driver argument.

Specialized versions of these functions exist for all of the supported tree
generator purities and visitor modes. This is done for two reasons: first, in
order to make the types more concrete to hopefully improve usability, and
second, because often the type of the tree generator is generic and so using a
specialized function automatically specializes the type rather than requiring
type annotation. The convention is `mainForVisitTreeXY` where `X` is empty for
pure generators, `IO` for generators running in the IO monad, and 'Impure' for
generators running in some general monad.
 -}

{-| Visit the given tree in parallel using the given pure generator; the results
    in the leaves will be summed up using the 'Monoid' instance.
 -}
mainForVisitTree ::
    (Monoid result, Serialize result, MonadIO result_monad) ⇒
    AllModePureGeneratorDriver tree_generator_configuration result_monad result {-^ the driver for the desired back-end (note that all drivers can be specialized to this type) -} →
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
mainForVisitTree = genericMain AllMode Pure

{-| Visit the given tree in parallel using the given generator in the IO monad;
    the results in the leaves will be summed up using the 'Monoid' instance.
 -}
mainForVisitTreeIO ::
    (Monoid result, Serialize result, MonadIO result_monad) ⇒
    AllModeIOGeneratorDriver tree_generator_configuration result_monad result {-^ the driver for the desired back-end (note that all drivers can be specialized to this type) -} →
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
mainForVisitTreeIO = genericMain AllMode io_purity

{-| Visit the given tree in parallel using the given impure generator; the
    results in all of the leaves will be summed up using the 'Monoid' instance.
 -}
mainForVisitTreeImpure ::
    (Monoid result, Serialize result, MonadIO result_monad, Functor m, MonadIO m) ⇒
    (∀ β. m β → IO β) →
    AllModeImpureGeneratorDriver tree_generator_configuration result_monad m result {-^ the driver for the desired back-end (note that all drivers can be specialized to this type) -} →
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
mainForVisitTreeImpure = genericMain AllMode . ImpureAtopIO

{-| This function is just like those above except that it is generalized over
    all tree generator purities and visitor modes.
 -}
genericMain ::
    ( MonadIO result_monad
    , ResultFor visitor_mode ~ result
    , Serialize (ProgressFor visitor_mode)
    ) ⇒
    VisitorMode visitor_mode {-^ the visitor mode -} →
    Purity m n {-^ the purity of the tree generator -} →
    Driver
        result_monad
        (SharedConfiguration tree_generator_configuration)
        SupervisorConfiguration
        m n
        visitor_mode
        {-^ the driver for the desired back-end (note that all drivers can be specialized to this type) -} →
    Term tree_generator_configuration {-^ a term with any configuration information needed to construct the tree generator -} →
    TermInfo
        {-^ information about the program; should look something like the following:

                > defTI { termDoc = "count the number of n-queens solutions for a given board size" }
         -} →
    (tree_generator_configuration → RunOutcomeFor visitor_mode → IO ())
        {-^ a callback that will be invoked with the outcome of the run (as well
            as the tree generator configuration information);  note that if the
            run was 'Completed' then the checkpoint file will be deleted if this
            function finishes successfully
         -} →
    (tree_generator_configuration → TreeGeneratorT m result) {-^ constructs the tree generator given the tree generator configuration information -} →
    result_monad ()
genericMain visitor_mode purity (Driver run) tree_generator_configuration_term program_info notifyTerminated_ constructTreeGenerator_ =
    run DriverParameters{..}
  where
    shared_configuration_term = makeSharedConfigurationTerm tree_generator_configuration_term
    supervisor_configuration_term =
        SupervisorConfiguration
            <$> checkpoint_configuration_term
            <*> statistics_configuration_term
    initializeGlobalState SharedConfiguration{logging_configuration=LoggingConfiguration{..}} =
        updateGlobalLogger rootLoggerName (setLevel log_level)
    constructTreeGenerator = constructTreeGenerator_ . tree_generator_configuration
    getStartingProgress _ SupervisorConfiguration{..} =
        case maybe_checkpoint_configuration of
            Nothing → (infoM "Checkpointing is NOT enabled") >> return (initialProgress visitor_mode)
            Just CheckpointConfiguration{..} → do
                noticeM $ "Checkpointing enabled"
                noticeM $ "Checkpoint file is " ++ checkpoint_path
                noticeM $ "Checkpoint interval is " ++ show checkpoint_interval ++ " seconds"
                ifM (doesFileExist checkpoint_path)
                    (noticeM "Loading existing checkpoint file" >> either error id . decodeLazy <$> readFile checkpoint_path)
                    (return $ initialProgress visitor_mode)
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
    , Serialize (ProgressFor (VisitorModeFor m))
    ) ⇒ CheckpointConfiguration → m α
checkpointLoop CheckpointConfiguration{..} = forever $ do
    liftIO $ threadDelay delay
    requestProgressUpdate >>= writeCheckpointFile checkpoint_path
  where
    delay = round $ checkpoint_interval * 1000000

managerLoop ::
    ( RequestQueueMonad m
    , Serialize (ProgressFor (VisitorModeFor m))
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
