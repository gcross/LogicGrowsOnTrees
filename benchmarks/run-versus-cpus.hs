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
import Control.Monad.Trans.Visitor.Parallel.Threads

import Criterion.Main
-- }}}

sumtree :: Int → Visitor (Sum Int)
sumtree 0 = return (Sum 1)
sumtree n = sumtree (n-1) `mplus` sumtree (n-1)

runWithCPUs :: Int → Int → IO ()
runWithCPUs depth number_of_workers = do
    result_mvar ← newEmptyMVar
    controller ← runVisitor (putMVar result_mvar) (sumtree depth)
    changeNumberOfWorkersAsync (const (return number_of_workers)) controller (void . return)
    takeMVar result_mvar >>= \x → case x of
        Completed (getSum → result) → if result == shiftL 1 depth then return () else error $ "result == " ++ show result ++ ", but should be " ++ show (shiftL (1::Int) depth)
        Aborted progress → error $ "aborted with progress " ++ show progress
        Failure exception → throwIO exception
    return ()

main = defaultMain $ map (b 20) [1..8]
  where
    b depth number_of_workers = bench (show number_of_workers ++ " workers") (runWithCPUs depth number_of_workers)