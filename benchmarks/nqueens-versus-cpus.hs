-- Language extensions {{{
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- }}}

-- Imports {{{
import Control.Concurrent.MVar
import Control.Monad

import Data.Bits
import Data.Monoid

import Control.Visitor (Visitor)
import Control.Visitor.Examples.Queens
import Control.Visitor.Parallel.Threads

import Criterion.Main
-- }}}

runWithCPUs :: Int → Int → IO ()
runWithCPUs n number_of_workers = do
    let correct_count = nqueensCorrectCount n
    result_mvar ← newEmptyMVar
    controller ← runVisitor (putMVar result_mvar) (nqueensCount n)
    changeNumberOfWorkersAsync (const (return number_of_workers)) controller (void . return)
    takeMVar result_mvar >>= \x → case x of
        Completed (getSum → result) → if result == correct_count then return () else error $ "result == " ++ show result ++ ", but should be " ++ show correct_count
        Aborted progress → error $ "aborted with progress " ++ show progress
        Failure message → error message
    return ()

main = defaultMain $ map (b 11) [1..4]
  where
    b n number_of_workers = bench (show number_of_workers ++ " workers") (runWithCPUs n number_of_workers)