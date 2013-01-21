-- Language extensions {{{
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

-- Imports {{{
import Control.Concurrent.MVar
import Control.Monad

import Data.Functor
import Data.Monoid (getSum)

import GHC.Conc

import System.Environment
import System.Log.Logger

import Control.Monad.Trans.Visitor.Examples.Queens
import Control.Monad.Trans.Visitor.Parallel.Threads
-- }}}

main = do
    -- updateGlobalLogger rootLoggerName (setLevel DEBUG) 
    n ← read . head <$> getArgs
    number_of_workers ← getNumCapabilities
    result_mvar ← newEmptyMVar
    controller ← runVisitor (putMVar result_mvar) (nqueensCount n)
    changeNumberOfWorkersAsync (const (return number_of_workers)) controller (void . return)
    takeMVar result_mvar >>= \x → case x of
        Completed (getSum → result) → print result
        Aborted progress → error $ "aborted with progress " ++ show progress
        Failure message → error message
