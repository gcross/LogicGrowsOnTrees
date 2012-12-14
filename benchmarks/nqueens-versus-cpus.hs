-- Language extensions {{{
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- }}}

-- Imports {{{
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad

import Data.Bits
import Data.Monoid

import Control.Monad.Trans.Visitor (Visitor)
import Control.Monad.Trans.Visitor.Examples.Queens
import Control.Monad.Trans.Visitor.Parallel.Threads

import Criterion.Main
-- }}}

runWithCPUs :: Int → Int → IO ()
runWithCPUs n number_of_workers = do
    result_mvar ← newEmptyMVar
    controller ← runVisitor (putMVar result_mvar) (fmap (const $ Sum (1::Int)) $ nqueens n)
    changeNumberOfWorkersAsync (const (return number_of_workers)) controller (void . return)
    takeMVar result_mvar >>= \x → case x of
        Completed (getSum → result) → if result == correct_counts !! n then return () else error $ "result == " ++ show result ++ ", but should be " ++ show (correct_counts !! n)
        Aborted progress → error $ "aborted with progress " ++ show progress
        Failure exception → throwIO exception
    return ()
  where
    correct_counts = [0,1,0,0,2,10,4,40,92,352,724,2680,14200,73712,365596,2279184,14772512,95815104,666090624]

main = defaultMain $ map (b 11) [1..4]
  where
    b n number_of_workers = bench (show number_of_workers ++ " workers") (runWithCPUs n number_of_workers)