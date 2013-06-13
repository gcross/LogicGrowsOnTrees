-- Language extensions {{{
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
-- }}}

module Visitor.Parallel.Main -- {{{
    ( AllModePureKindDriver
    , AllModeIOKindDriver
    , AllModeImpureKindDriver
    , Driver(..)
    , RunOutcome(..)
    , RunOutcomeFor
    , TerminationReason(..)
    , TerminationReasonFor
    , extractRunOutcomeFromSupervisorOutcome
    , mainParser
    , mainVisitor
    , mainVisitorIO
    , mainVisitorT
    ) where -- }}}

-- Imports {{{
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
import Visitor.Parallel.Common.Supervisor -- {{{
    ( FunctionOfTimeStatistics(..)
    , IndependentMeasurementsStatistics(..)
    , RunStatistics(..)
    , SupervisorTerminationReason(..)
    , SupervisorOutcome(..)
    )
-- }}}
import Visitor.Parallel.Common.Supervisor.RequestQueue
import Visitor.Parallel.Common.VisitorMode
import Visitor.Parallel.Common.Worker
-- }}}

-- Logging Functions {{{
deriveLoggers "Logger" [INFO,NOTICE]
-- }}}

-- Types {{{

-- Configuration {{{
data CheckpointConfiguration = CheckpointConfiguration -- {{{
    {   checkpoint_path :: FilePath
    ,   checkpoint_interval :: Float
    } deriving (Eq,Show)
-- }}}

data LoggingConfiguration = LoggingConfiguration -- {{{
    {   log_level :: Priority
    } deriving (Eq,Show)
instance Serialize LoggingConfiguration where
    put = put . show . log_level
    get = LoggingConfiguration . read <$> get
-- }}}

data StatisticsConfiguration = StatisticsConfiguration -- {{{
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
-- }}}

data SupervisorConfiguration = SupervisorConfiguration -- {{{
    {   maybe_checkpoint_configuration :: Maybe CheckpointConfiguration
    ,   statistics_configuration :: StatisticsConfiguration
    } deriving (Eq,Show)
-- }}}

data SharedConfiguration visitor_configuration = SharedConfiguration -- {{{
    {   logging_configuration :: LoggingConfiguration
    ,   visitor_configuration :: visitor_configuration
    } deriving (Eq,Show)
$( derive makeSerialize ''SharedConfiguration )
-- }}}
-- }}}

data Driver -- {{{
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
        VisitorMode visitor_mode →
        VisitorKind m n →
        Term shared_configuration →
        Term supervisor_configuration →
        TermInfo →
        (shared_configuration → IO ()) →
        (shared_configuration → TreeGeneratorT m (ResultFor visitor_mode)) →
        (shared_configuration → supervisor_configuration → IO (ProgressFor visitor_mode)) →
        (shared_configuration → supervisor_configuration → RunOutcomeFor visitor_mode → IO ()) →
        (shared_configuration → supervisor_configuration → manager_monad visitor_mode ()) →
        result_monad ()
    )
-- }}}

-- Driver type aliases {{{
type AllModePureKindDriver visitor_configuration result_monad result = -- {{{
    Driver
        result_monad
        (SharedConfiguration visitor_configuration)
        SupervisorConfiguration
        Identity IO
        (AllMode result)
-- }}}
type AllModeIOKindDriver visitor_configuration result_monad result = -- {{{
    Driver
        result_monad
        (SharedConfiguration visitor_configuration)
        SupervisorConfiguration
        IO IO
        (AllMode result)
-- }}}
type AllModeImpureKindDriver visitor_configuration result_monad m result = -- {{{
    Driver
        result_monad
        (SharedConfiguration visitor_configuration)
        SupervisorConfiguration
        m m
        (AllMode result)
-- }}}
-- }}}

data RunOutcome progress final_result = RunOutcome -- {{{
    {   runStatistics :: RunStatistics
    ,   runTerminationReason :: TerminationReason progress final_result
    } deriving (Eq,Show)
-- }}}
type RunOutcomeFor visitor_mode = RunOutcome (ProgressFor visitor_mode) (FinalResultFor visitor_mode)

data TerminationReason progress final_result = -- {{{
    Aborted progress
  | Completed final_result
  | Failure String
  deriving (Eq,Show)
-- }}}
type TerminationReasonFor visitor_mode = TerminationReason (ProgressFor visitor_mode) (FinalResultFor visitor_mode)

-- }}}

-- Instances {{{
instance ArgVal Priority where -- {{{
    converter = enum $
        [DEBUG,INFO,NOTICE,WARNING,ERROR,CRITICAL,ALERT,EMERGENCY]
        >>=
        \level → let name = show level
                 in return (name,level) `mplus` return (map toLower name,level)
-- }}}
-- }}}

-- Values {{{
-- Terms {{{
checkpoint_configuration_term :: Term (Maybe CheckpointConfiguration) -- {{{
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
-- }}}

logging_configuration_term :: Term LoggingConfiguration -- {{{
logging_configuration_term =
    LoggingConfiguration
    <$> value (flip opt (
        (optInfo ["l","log-level"])
        {   optName = "LEVEL"
        ,   optDoc = "This specifies the upper bound (inclusive) on the importance of the messages that will be logged;  it must be one of (in increasing order of importance): DEBUG, INFO, NOTICE, WARNING, ERROR, CRITICAL, ALERT, or EMERGENCY."
        }
        ) WARNING)
-- }}}

statistics_configuration_term :: Term StatisticsConfiguration -- {{{
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
-- }}}

supervisor_configuration_term :: Term SupervisorConfiguration -- {{{
supervisor_configuration_term =
    SupervisorConfiguration
        <$> checkpoint_configuration_term
        <*> statistics_configuration_term
-- }}}

makeSharedConfigurationTerm :: Term visitor_configuration → Term (SharedConfiguration visitor_configuration) -- {{{
makeSharedConfigurationTerm visitor_configuration_term =
    SharedConfiguration
        <$> logging_configuration_term
        <*> visitor_configuration_term
-- }}}
-- }}}
-- }}}

-- Exposed Functions {{{

extractRunOutcomeFromSupervisorOutcome :: -- {{{
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
-- }}}

mainParser :: Term α → TermInfo → IO α -- {{{
mainParser term term_info =
    (if null (termName term_info)
        then getProgName >>= \progname → return $ term_info {termName = progname}
        else return term_info
    ) >>= exec . (term,)
-- }}}

mainVisitor :: -- {{{
    (Monoid result, Serialize result, MonadIO result_monad) ⇒
    AllModePureKindDriver visitor_configuration result_monad result →
    Term visitor_configuration →
    TermInfo →
    (visitor_configuration → RunOutcome (Progress result) result → IO ()) →
    (visitor_configuration → TreeGenerator result) →
    result_monad ()
mainVisitor = genericMain AllMode PureVisitor
-- }}}

mainVisitorIO :: -- {{{
    (Monoid result, Serialize result, MonadIO result_monad) ⇒
    AllModeIOKindDriver visitor_configuration result_monad result →
    Term visitor_configuration →
    TermInfo →
    (visitor_configuration → RunOutcome (Progress result) result → IO ()) →
    (visitor_configuration → TreeGeneratorIO result) →
    result_monad ()
mainVisitorIO = genericMain AllMode IOVisitor
-- }}}

mainVisitorT :: -- {{{
    (Monoid result, Serialize result, MonadIO result_monad, Functor m, MonadIO m) ⇒
    (∀ β. m β → IO β) →
    AllModeImpureKindDriver visitor_configuration result_monad m result →
    Term visitor_configuration →
    TermInfo →
    (visitor_configuration → RunOutcome (Progress result) result → IO ()) →
    (visitor_configuration → TreeGeneratorT m result) →
    result_monad ()
mainVisitorT = genericMain AllMode . ImpureVisitor
-- }}}

-- }}}

-- Internal Functions {{{

-- Loops {{{
checkpointLoop :: -- {{{
    ( RequestQueueMonad m
    , Serialize (ProgressFor (VisitorModeFor m))
    ) ⇒ CheckpointConfiguration → m α
checkpointLoop CheckpointConfiguration{..} = forever $ do
    liftIO $ threadDelay delay
    requestProgressUpdate >>= writeCheckpointFile checkpoint_path
  where
    delay = round $ checkpoint_interval * 1000000
-- }}}

managerLoop :: -- {{{
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
-- }}}
-- }}}

genericMain :: -- {{{
    ( MonadIO result_monad
    , ResultFor visitor_mode ~ result
    , Serialize (ProgressFor visitor_mode)
    ) ⇒
    VisitorMode visitor_mode →
    VisitorKind m n →
    Driver
        result_monad
        (SharedConfiguration visitor_configuration)
        SupervisorConfiguration
        m n
        visitor_mode →
    Term visitor_configuration →
    TermInfo →
    (visitor_configuration → RunOutcomeFor visitor_mode → IO ()) →
    (visitor_configuration → TreeGeneratorT m result) →
    result_monad ()
genericMain visitor_mode visitor_kind (Driver run) visitor_configuration_term infomod notifyTerminated constructVisitor =
    run  visitor_mode
         visitor_kind
        (makeSharedConfigurationTerm visitor_configuration_term)
         supervisor_configuration_term
         infomod
        (\SharedConfiguration{logging_configuration=LoggingConfiguration{..}} → do
            updateGlobalLogger rootLoggerName (setLevel log_level)
        )
        (constructVisitor . visitor_configuration)
        (\_ SupervisorConfiguration{..} →
            case maybe_checkpoint_configuration of
                Nothing → (infoM "Checkpointing is NOT enabled") >> return (initialProgress visitor_mode)
                Just CheckpointConfiguration{..} → do
                    noticeM $ "Checkpointing enabled"
                    noticeM $ "Checkpoint file is " ++ checkpoint_path
                    noticeM $ "Checkpoint interval is " ++ show checkpoint_interval ++ " seconds"
                    ifM (doesFileExist checkpoint_path)
                        (noticeM "Loading existing checkpoint file" >> either error id . decodeLazy <$> readFile checkpoint_path)
                        (return $ initialProgress visitor_mode)
        )
        (\SharedConfiguration{..} SupervisorConfiguration{..} run_outcome@RunOutcome{..} →
            (do showStatistics statistics_configuration runStatistics
                notifyTerminated visitor_configuration run_outcome
            ) `finally`
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
        )
        (const managerLoop)
-- }}}

maybeForkIO :: RequestQueueMonad m ⇒ (α → m ()) → Maybe α → m (Maybe ThreadId) -- {{{
maybeForkIO loop = maybe (return Nothing) (liftM Just . fork . loop)
-- }}}

removeFileIfExists :: FilePath → IO () -- {{{
removeFileIfExists path =
    handleJust
        (\e → if isDoesNotExistError e then Nothing else Just ())
        (\_ → return ())
        (removeFile path)
-- }}}

showStatistics :: MonadIO m ⇒ StatisticsConfiguration → RunStatistics → m () -- {{{
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
          if statCount == 0
            then
              "At no point did a worker receive a new workload after finishing a workload."
            else
              if statMax == 0
                then
                  printf "Workers completed their task and obtained a new workload %i times and never had to wait to receive the new workload."
                    statCount
                else
                  printf
                    (unlines
                        ["Workers completed their task and obtained a new workload %i times with an average of one every %sseconds or %.1g enqueues/second."
                        ,"The minimum waiting time was %sseconds, and the maximum waiting time was %sseconds."
                        ,"On average, a worker had to wait %sseconds +/- %sseconds (std. dev) for a new workload."
                        ]
                    )
                    statCount
                    (showWithUnitPrefix $ total_time / fromIntegral statCount)
                    (fromIntegral statCount / total_time)
                    (showWithUnitPrefix statMin)
                    (showWithUnitPrefix statMax)
                    (showWithUnitPrefix statAverage)
                    (showWithUnitPrefix statStdDev)
    when show_steal_wait_times $ do
        let IndependentMeasurementsStatistics{..} = runStealWaitTimes
        hPutStrLn stderr $
          if timeCount == 0
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
                timeCount
                (showWithUnitPrefix $ total_time / fromIntegral timeCount)
                (fromIntegral timeCount / total_time)
                (showWithUnitPrefix timeMin)
                (showWithUnitPrefix timeMax)
                (showWithUnitPrefix timeMean)
                (showWithUnitPrefix timeStdDev)
    when show_numbers_of_waiting_workers $ do
        let FunctionOfTimeStatistics{..} = runWaitingWorkerStatistics
        hPutStrLn stderr $
          if statMax == 0
            then
              printf "No worker ever had to wait for a workload to become available.\n"
            else if statMin == 0
              then
                printf "On average, %.1f +/ - %.1f (std. dev) workers were waiting at any given time;  never more than %i.\n"
                  statAverage
                  statStdDev
                  statMax
              else
                printf "On average, %.1f +/ - %.1f (std. dev) workers were waiting at any given time;  never more than %i nor fewer than %i.\n"
                  statAverage
                  statStdDev
                  statMax
                  statMin
    when show_numbers_of_available_workloads $ do
        let FunctionOfTimeStatistics{..} = runAvailableWorkloadStatistics
        hPutStrLn stderr $
          if statMax == 0
            then
              printf "No workload ever had to wait for an available worker.\n"
            else if statMin == 0
              then
                printf "On average, %.1f +/ - %.1f (std. dev) workloads were waiting for a worker at any given time;  never more than %i.\n"
                  statAverage
                  statStdDev
                  statMax
              else
                printf "On average, %.1f +/ - %.1f (std. dev) workloads were waiting for a worker at any given time;  never more than %i nor fewer than %i.\n"
                  statAverage
                  statStdDev
                  statMax
                  statMin
    when show_instantaneous_workload_request_rates $ do
        let FunctionOfTimeStatistics{..} = runInstantaneousWorkloadRequestRateStatistics
        hPutStrLn stderr $
            printf
                (unlines
                    ["On average, the instantanenous rate at which workloads were being requested was %.1f +/ - %.1f (std. dev) requests per second;  the rate never fell below %.1f nor rose above %.1f."
                    ,"This value was obtained by exponentially smoothing the request data over a time scale of one second."
                    ]
                )
                statAverage
                statStdDev
                statMin
                statMax
    when show_instantaneous_workload_steal_times $ do
        let FunctionOfTimeStatistics{..} = runInstantaneousWorkloadStealTimeStatistics
        hPutStrLn stderr $
            printf
                (unlines
                    ["On average, the instantaneous time to steal a workload was %sseconds +/ - %sseconds (std. dev);  this time interval never fell below %sseconds nor rose above %sseconds."
                    ,"This value was obtained by exponentially smoothing the request data over a time scale of one second."
                    ]
                )
                (showWithUnitPrefix statAverage)
                (showWithUnitPrefix statStdDev)
                (showWithUnitPrefix statMin)
                (showWithUnitPrefix statMax)
    when show_buffer_size $ do
        let FunctionOfTimeStatistics{..} = runBufferSizeStatistics
        hPutStrLn stderr $
            printf "On average, the buffer size was %.1f +/ - %.1f (std. dev);  it was never smaller than %i, nor greater than %i.\n"
                statAverage
                statStdDev
                statMin
                statMax
  where
    showWithUnitPrefix :: Real n ⇒ n → String
    showWithUnitPrefix 0 = "0 "
    showWithUnitPrefix x = printf "%.1f %s" x_scaled (unitName unit)
      where
        (x_scaled :: Float,Just unit) = formatValue (Left FormatSiAll) . realToFrac $ x
-- }}}

writeCheckpointFile :: (Serialize ip, MonadIO m) ⇒ FilePath → ip → m () -- {{{
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
-- }}}

-- }}}
