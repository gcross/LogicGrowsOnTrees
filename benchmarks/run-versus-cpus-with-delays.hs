-- Language extensions {{{
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- }}}

-- Imports {{{
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class

import Data.Bits
import Data.Monoid

import Control.Visitor (VisitorIO)
import Control.Visitor.Parallel.Threads

import System.Log.Logger

import Criterion.Main
-- }}}

sumtree :: Int → VisitorIO (Sum Int)
sumtree 0 = liftIO (threadDelay 4) >> return (Sum 1)
sumtree n = liftIO (threadDelay 4) >> (sumtree (n-1) `mplus` sumtree (n-1))

runWithCPUs :: Int → Int → IO ()
runWithCPUs depth number_of_workers = do
    result_mvar ← newEmptyMVar
    controller ← runVisitorIO (putMVar result_mvar) (sumtree depth)
    changeNumberOfWorkersAsync (const (return number_of_workers)) controller (void . return)
    takeMVar result_mvar >>= \x → case x of
        Completed (getSum → result) → if result == shiftL 1 depth then return () else error $ "result == " ++ show result ++ ", but should be " ++ show (shiftL (1::Int) depth)
        Aborted progress → error $ "aborted with progress " ++ show progress
        Failure exception → throwIO exception
    return ()

main = defaultMain $ map (b 15) [1..4]
  where
    b depth number_of_workers = bench (show number_of_workers ++ " workers") (runWithCPUs depth number_of_workers)