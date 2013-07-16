-- Language extensions {{{
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- }}}

-- Imports {{{
import Prelude hiding (catch)

import Control.Exception (SomeException,catch)
import Control.Monad (forever,replicateM_,void)
import Control.Monad.IO.Class (liftIO)

import Data.Functor ((<$>))
import Data.IORef (modifyIORef,newIORef,readIORef)
import Data.Monoid ((<>),Sum(..),mempty)
import Data.Serialize (Serialize(..))

import System.Environment (getArgs)
import System.Log.Logger (Priority(..),updateGlobalLogger,rootLoggerName,setLevel)
import qualified System.Log.Logger as Logger -- needed for deriveLoggers
import System.Log.Logger.TH (deriveLoggers)
import System.IO (stdin,stdout)
import System.Random (randomRIO)

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import qualified Visitor as V
import Visitor.Checkpoint
import Visitor.Examples.Queens
import Visitor.Parallel.Adapter.Processes
import Visitor.Parallel.Common.ExplorationMode (ExplorationMode(AllMode))
import Visitor.Parallel.Common.Process (runWorkerUsingHandles)
import Visitor.Parallel.Common.Worker(Purity(Pure))
import Visitor.Parallel.Main
-- }}}

-- Logging Functions {{{
deriveLoggers "Logger" [ERROR]
-- }}}

-- Helper functions {{{
remdups :: (Eq a) => [a] -> [a] -- {{{
remdups []  =  []
remdups (x : []) =  [x]
remdups (x : xx : xs)
 | x == xx   = remdups (x : xs)
 | otherwise = x : remdups (xx : xs)
-- }}}
-- }}}

-- Instances {{{
instance Serialize (Sum Int) where -- {{{
    put = put . getSum
    get = fmap Sum get
-- }}}
-- }}}

main = do
    -- updateGlobalLogger rootLoggerName (setLevel DEBUG)
    args ← getArgs
    case args of
        ["nqueens",read → n] →
            runWorkerUsingHandles
                AllMode
                Pure
                (nqueensCount n)
                stdin
                stdout
             `catch`
             (\(e::SomeException) → errorM $ "Worker process failed: " ++ show e)
        _ → defaultMain tests

tests = -- {{{
    [testCase "one process" . runTest . void $ do
        changeNumberOfWorkers (return . (\i → 0))
        changeNumberOfWorkers (return . (\i → 1))
    ,testCase "two processes" . runTest . void $
        changeNumberOfWorkers (return . (\i → 3-i))
    ,testCase "many processes" . runTest . void $ liftIO (randomRIO (0,1::Int)) >>= \i → case i of
        0 → changeNumberOfWorkers (return . (\i → if i > 1 then i-1 else i))
        1 → changeNumberOfWorkers (return . (+1))
    ]
  where
    runTest generateNoise = do
        let n = 15
            tree = nqueensCount n
        progresses_ref ← newIORef []
        filepath ← getProgFilepath
        RunOutcome _ termination_reason ←
            runSupervisor
                AllMode
                filepath
                ["nqueens",show n]
                (const $ return ())
                mempty
                (forever $ requestProgressUpdate >>= (liftIO . modifyIORef progresses_ref . (:)) >> generateNoise)
        result ← case termination_reason of
            Aborted _ → error "prematurely aborted"
            Completed result → return result
            Failure message → error message
        let correct_result = V.exploreTree tree
        result @?= correct_result
        progresses ← remdups <$> readIORef progresses_ref
        replicateM_ 4 $ randomRIO (0,length progresses-1) >>= \i → do
            let Progress checkpoint result = progresses !! i
            result @=? exploreTreeStartingFromCheckpoint (invertCheckpoint checkpoint) tree
            correct_result @=? result <> (exploreTreeStartingFromCheckpoint checkpoint tree)
-- }}}
