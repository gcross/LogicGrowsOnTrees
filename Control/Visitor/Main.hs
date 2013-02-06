-- Language extensions {{{
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Control.Visitor.Main -- {{{
    ( Driver(..)
    , TerminationReason(..)
    , mainVisitor
    , mainVisitorIO
    , mainVisitorT
    ) where -- }}}

-- Imports {{{
import Prelude hiding (readFile,writeFile)

import Control.Concurrent (ThreadId,killThread,threadDelay)
import Control.Exception (finally,handleJust,onException)
import Control.Monad (forever,liftM)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Tools (ifM)

import Data.ByteString.Lazy (readFile,writeFile)
import Data.Char (toUpper)
import Data.Composition ((.*))
import Data.Derive.Serialize
import Data.DeriveTH
import Data.Either.Unwrap (mapRight)
import Data.Maybe (catMaybes)
import Data.Monoid (Monoid(..))
import Data.Serialize

import Options.Applicative

import System.Directory (doesFileExist,removeFile,renameFile)
import System.IO.Error (isDoesNotExistError)
import qualified System.Log.Logger as Logger
import System.Log.Logger (Priority(DEBUG,INFO,NOTICE,WARNING),setLevel,rootLoggerName,updateGlobalLogger)
import System.Log.Logger.TH

import Control.Visitor (Visitor,VisitorIO,VisitorT)
import Control.Visitor.Checkpoint
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

data Configuration = Configuration -- {{{
    {   maybe_checkpoint_configuration :: Maybe CheckpointConfiguration
    ,   logging_configuration :: LoggingConfiguration
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
            (VisitorWorkerTerminationReason result → IO ()) →
            visitor result →
            VisitorWorkload →
            IO (VisitorWorkerEnvironment result)
        ) →
        Parser configuration →
        (∀ α. InfoMod α) →
        (configuration → IO ()) →
        (configuration → IO (Maybe (Progress result))) →
        (configuration → TerminationReason result → IO ()) →
        (configuration → visitor result) →
        (configuration → manager_monad result ()) →
        result_monad ()
    )

data TerminationReason result = -- {{{
    Aborted (Progress result)
  | Completed result
  | Failure String
  deriving (Eq,Show)
-- }}}

-- }}}

-- Values {{{
-- Options {{{
checkpoint_configuration_options :: Parser (Maybe CheckpointConfiguration) -- {{{
checkpoint_configuration_options =
    maybe (const Nothing) (Just .* CheckpointConfiguration)
        <$> nullOption
            (   long "checkpoint-file"
             <> metavar "FILE"
             <> short 'c'
             <> help "Path to the checkpoint file;  enables periodic checkpointing"
             <> reader (Right . Just)
             <> value Nothing
            )
        <*> option
            (   long "checkpoint-interval"
             <> metavar "SECONDS"
             <> short 'i'
             <> help "Time between checkpoints (in seconds, decimals allowed);  ignored if checkpoint file not specified"
             <> value 60
             <> showDefault
            )
-- }}}

logging_configuration_options :: Parser LoggingConfiguration -- {{{
logging_configuration_options =
    LoggingConfiguration
        <$> nullOption
            (   long "log-level"
             <> metavar "LEVEL"
             <> short 'l'
             <> help "Upper bound (inclusive) on the importance of the messages that will be logged;  must be one of (in increasing order of importance): DEBUG, INFO, NOTICE, WARNING, ERROR, CRITICAL, ALERT, EMERGENCY"
             <> value WARNING
             <> showDefault
             <> reader (auto . map toUpper)
            )
-- }}}

configuration_options :: Parser Configuration -- {{{
configuration_options =
    Configuration
        <$> checkpoint_configuration_options
        <*> logging_configuration_options
-- }}}
-- }}}
-- }}}

-- Exposed Functions {{{

mainVisitor :: -- {{{
    (Monoid result, Serialize result, MonadIO result_monad) ⇒
    Driver result_monad (Configuration,visitor_configuration) Visitor result →
    Parser visitor_configuration →
    (∀ α. InfoMod α) →
    (visitor_configuration → TerminationReason result → IO ()) →
    (visitor_configuration → Visitor result) →
    result_monad ()
mainVisitor (Driver runDriver) = genericMain . runDriver $ forkVisitorWorkerThread
-- }}}

mainVisitorIO :: -- {{{
    (Monoid result, Serialize result, MonadIO result_monad) ⇒
    Driver result_monad (Configuration,visitor_configuration) VisitorIO result →
    Parser visitor_configuration →
    (∀ α. InfoMod α) →
    (visitor_configuration → TerminationReason result → IO ()) →
    (visitor_configuration → VisitorIO result) →
    result_monad ()
mainVisitorIO (Driver runDriver) = genericMain . runDriver $ forkVisitorIOWorkerThread
-- }}}

mainVisitorT :: -- {{{
    (Monoid result, Serialize result, MonadIO result_monad, Functor m, MonadIO m) ⇒
    Driver result_monad (Configuration,visitor_configuration) (VisitorT m) result →
    (∀ β. m β → IO β) →
    Parser visitor_configuration →
    (∀ α. InfoMod α) →
    (visitor_configuration → TerminationReason result → IO ()) →
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
        Parser (Configuration,visitor_configuration) →
        (∀ α. InfoMod α) →
        ((Configuration,visitor_configuration) → IO ()) →
        ((Configuration,visitor_configuration) → IO (Maybe (Progress result))) →
        ((Configuration,visitor_configuration) → TerminationReason result → IO ()) →
        ((Configuration,visitor_configuration) → visitor) →
        ((Configuration,visitor_configuration) → manager_monad result ()) →
        result_monad ()
    ) →
    Parser visitor_configuration →
    (∀ α. InfoMod α) →
    (visitor_configuration → TerminationReason result → IO ()) →
    (visitor_configuration → visitor) →
    result_monad ()
genericMain run visitor_configuration_options infomod notifyTerminated constructVisitor =
    run (liftA2 (,) configuration_options visitor_configuration_options)
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
        (\(Configuration{..},visitor_configuration) termination_reason →
            notifyTerminated visitor_configuration termination_reason
            `finally`
            case maybe_checkpoint_configuration of
                Nothing → return ()
                Just CheckpointConfiguration{checkpoint_path} →
                    let deleteCheckpointFile = do
                            noticeM "Deleting any remaining checkpoint file"
                            removeFileIfExists checkpoint_path
                    in case termination_reason of
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
