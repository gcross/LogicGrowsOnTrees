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

module Control.Visitor.Main -- {{{
    ( Driver(..)
    , RunOutcome(..)
    , TerminationReason(..)
    , mainParser
    , mainVisitor
    , mainVisitorIO
    , mainVisitorT
    ) where -- }}}

-- Imports {{{
import Prelude hiding (readFile,writeFile)

import Control.Applicative ((<$>),(<*>),liftA2)
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
import Data.Either.Unwrap (mapRight)
import Data.Maybe (catMaybes)
import Data.Monoid (Endo(..),Monoid(..))
import Data.Prefix.Units (FormatMode(FormatSiAll),fancySymbol,formatValue,unitName)
import Data.Serialize
import Data.Traversable (sequenceA)

import System.Console.CmdTheLine
import System.Directory (doesFileExist,removeFile,renameFile)
import System.Environment (getArgs,getProgName)
import System.Exit (exitWith)
import System.IO (hPutStr,hPutStrLn,stderr)
import System.IO.Error (isDoesNotExistError)
import qualified System.Log.Logger as Logger
import System.Log.Logger (Priority(..),setLevel,rootLoggerName,updateGlobalLogger)
import System.Log.Logger.TH

import Text.Printf (printf)

import Control.Visitor (Visitor,VisitorIO,VisitorT)
import Control.Visitor.Checkpoint
import Control.Visitor.Supervisor (RunStatistics(..),Statistics(..),TimeStatistics(..))
import Control.Visitor.Supervisor.RequestQueue
import Control.Visitor.Worker
import Control.Visitor.Workload
-- }}}

-- Logging Functions {{{
deriveLoggers "Logger" [DEBUG,INFO,NOTICE]
-- }}}

-- Types {{{

-- Configuration {{{
data CheckpointConfiguration = CheckpointConfiguration -- {{{
    {   checkpoint_path :: FilePath
    ,   checkpoint_interval :: Float
    } deriving (Eq,Show)
$( derive makeSerialize ''CheckpointConfiguration )
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
$( derive makeSerialize ''StatisticsConfiguration )
-- }}}

data Configuration = Configuration -- {{{
    {   maybe_checkpoint_configuration :: Maybe CheckpointConfiguration
    ,   logging_configuration :: LoggingConfiguration
    ,   statistics_configuration :: StatisticsConfiguration
    } deriving (Eq,Show)
$( derive makeSerialize ''Configuration )
-- }}}
-- }}}

data Driver result_monad configuration visitor result =  -- {{{
    ∀ manager_monad.
    ( RequestQueueMonad (manager_monad result)
    , RequestQueueMonadResult (manager_monad result) ~ result
    ) ⇒
    Driver (
        ( Monoid result
        , Serialize result
        , MonadIO result_monad
        ) ⇒
        (
            (WorkerTerminationReason result → IO ()) →
            visitor result →
            Workload →
            IO (WorkerEnvironment result)
        ) →
        Term configuration →
        TermInfo →
        (configuration → IO ()) →
        (configuration → IO (Maybe (Progress result))) →
        (configuration → RunOutcome result → IO ()) →
        (configuration → visitor result) →
        (configuration → manager_monad result ()) →
        result_monad ()
    )
-- }}}

data RunOutcome result = RunOutcome -- {{{
    {   runStatistics :: RunStatistics
    ,   runTerminationReason :: TerminationReason result
    } deriving (Eq,Show)
-- }}}

data TerminationReason result = -- {{{
    Aborted (Progress result)
  | Completed result
  | Failure String
  deriving (Eq,Show)
-- }}}

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

configuration_term :: Term Configuration -- {{{
configuration_term =
    Configuration
        <$> checkpoint_configuration_term
        <*> logging_configuration_term
        <*> statistics_configuration_term
-- }}}
-- }}}
-- }}}

-- Exposed Functions {{{

mainParser :: Term α → TermInfo → IO α -- {{{
mainParser term term_info =
    (if null (termName term_info)
        then getProgName >>= \progname → return $ term_info {termName = progname}
        else return term_info
    ) >>= exec . (term,)
-- }}}

mainVisitor :: -- {{{
    (Monoid result, Serialize result, MonadIO result_monad) ⇒
    Driver result_monad (Configuration,visitor_configuration) Visitor result →
    Term visitor_configuration →
    TermInfo →
    (visitor_configuration → RunOutcome result → IO ()) →
    (visitor_configuration → Visitor result) →
    result_monad ()
mainVisitor (Driver runDriver) = genericMain . runDriver $ forkVisitorWorkerThread
-- }}}

mainVisitorIO :: -- {{{
    (Monoid result, Serialize result, MonadIO result_monad) ⇒
    Driver result_monad (Configuration,visitor_configuration) VisitorIO result →
    Term visitor_configuration →
    TermInfo →
    (visitor_configuration → RunOutcome result → IO ()) →
    (visitor_configuration → VisitorIO result) →
    result_monad ()
mainVisitorIO (Driver runDriver) = genericMain . runDriver $ forkVisitorIOWorkerThread
-- }}}

mainVisitorT :: -- {{{
    (Monoid result, Serialize result, MonadIO result_monad, Functor m, MonadIO m) ⇒
    Driver result_monad (Configuration,visitor_configuration) (VisitorT m) result →
    (∀ β. m β → IO β) →
    Term visitor_configuration →
    TermInfo →
    (visitor_configuration → RunOutcome result → IO ()) →
    (visitor_configuration → VisitorT m result) →
    result_monad ()
mainVisitorT (Driver runDriver) = genericMain . runDriver . forkVisitorTWorkerThread
-- }}}

-- }}}

-- Internal Functions {{{

-- Loops {{{
checkpointLoop :: (RequestQueueMonad m, Serialize (RequestQueueMonadResult m)) ⇒ CheckpointConfiguration → m α -- {{{
checkpointLoop CheckpointConfiguration{..} = forever $ do
    liftIO $ threadDelay delay
    requestProgressUpdate >>= writeCheckpointFile checkpoint_path
  where
    delay = round $ checkpoint_interval * 1000000
-- }}}

managerLoop :: (RequestQueueMonad m, Serialize (RequestQueueMonadResult m)) ⇒ Configuration → m () -- {{{
managerLoop Configuration{..} = do
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
    ( result ~ RequestQueueMonadResult (manager_monad result)
    , RequestQueueMonad (manager_monad result)
    , Serialize result
    , MonadIO result_monad
    ) ⇒
    (
        Term (Configuration,visitor_configuration) →
        TermInfo →
        ((Configuration,visitor_configuration) → IO ()) →
        ((Configuration,visitor_configuration) → IO (Maybe (Progress result))) →
        ((Configuration,visitor_configuration) → RunOutcome result → IO ()) →
        ((Configuration,visitor_configuration) → visitor) →
        ((Configuration,visitor_configuration) → manager_monad result ()) →
        result_monad ()
    ) →
    Term visitor_configuration →
    TermInfo →
    (visitor_configuration → RunOutcome result → IO ()) →
    (visitor_configuration → visitor) →
    result_monad ()
genericMain run visitor_configuration_term infomod notifyTerminated constructVisitor =
    run (liftA2 (,) configuration_term visitor_configuration_term)
         infomod
        (\(Configuration{logging_configuration=LoggingConfiguration{..}},_) →
            updateGlobalLogger rootLoggerName (setLevel log_level)
        )
        (\(Configuration{..},_) →
            case maybe_checkpoint_configuration of
                Nothing → (infoM "Checkpointing is NOT enabled") >> return Nothing
                Just CheckpointConfiguration{..} → do
                    noticeM $ "Checkpointing enabled"
                    noticeM $ "Checkpoint file is " ++ checkpoint_path
                    noticeM $ "Checkpoint interval is " ++ show checkpoint_interval ++ " seconds"
                    ifM (doesFileExist checkpoint_path)
                        (noticeM "Loading existing checkpoint file" >> either error Just . decodeLazy <$> readFile checkpoint_path)
                        (return Nothing)
        )
        (\(Configuration{..},visitor_configuration) run_outcome@RunOutcome{..} →
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
        (constructVisitor . snd)
        (managerLoop . fst)
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
        total_time = fromRational . toRational $ runWallTime
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
        let TimeStatistics{..} = runWorkerWaitTimes
        hPutStrLn stderr $
            printf
                (unlines
                    ["Workers requested new workloads %i times with an average of %sseconds between each request or %.1g requests/second."
                    ,"The minimum waiting time was %sseconds, and the maximum waiting time was %sseconds."
                    ,"On average, a worker had to wait %sseconds +/- %sseconds (std. dev) for a new workload."
                    ]
                )
                timeCount
                (showWithUnitPrefix $ total_time / fromIntegral timeCount)
                (fromIntegral timeCount / total_time)
                (showWithUnitPrefix timeMin)
                (showWithUnitPrefix timeMax)
                (showWithUnitPrefix timeMean)
                (showWithUnitPrefix timeStdDev)
    when show_steal_wait_times $ do
        let TimeStatistics{..} = runStealWaitTimes
        hPutStrLn stderr $
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
        let Statistics{..} = runWaitingWorkerStatistics
        hPutStrLn stderr $
            printf "On average, %.1f +/ - %.1f (std. dev) workers were waiting at any given time;  never fewer than %i.\n"
                statAverage
                statStdDev
                statMin
    when show_numbers_of_available_workloads $ do
        let Statistics{..} = runAvailableWorkloadStatistics
        hPutStrLn stderr $
            printf "On average, %.1f +/ - %.1f (std. dev) workloads were available at any given time;  never fewer than %i, nor more than %i.\n"
                statAverage
                statStdDev
                statMin
                statMax
    when show_instantaneous_workload_request_rates $ do
        let Statistics{..} = runInstantaneousWorkloadRequestRateStatistics
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
        let Statistics{..} = runInstantaneousWorkloadStealTimeStatistics
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
        let Statistics{..} = runBufferSizeStatistics
        hPutStrLn stderr $
            printf "On average, the buffer size was %.1f +/ - %.1f (std. dev);  it was never smaller than %i, nor greater than %i.\n"
                statAverage
                statStdDev
                statMin
                statMax
  where
    showWithUnitPrefix :: Real n ⇒ n → String
    showWithUnitPrefix x = printf "%.1f %s" x_scaled (unitName unit)
      where
        (x_scaled :: Float,Just unit) = formatValue (Left FormatSiAll) . fromRational . toRational $ x 
-- }}}

writeCheckpointFile :: (Serialize result, MonadIO m) ⇒ FilePath → Progress result → m () -- {{{
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
