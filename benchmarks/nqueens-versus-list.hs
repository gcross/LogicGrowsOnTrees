-- Language extensions {{{
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

-- Imports {{{
import Control.Concurrent.MVar
import Control.Monad
import Criterion.Main
import Data.Monoid

import Control.Visitor
import Control.Visitor.Checkpoint
import Control.Visitor.Examples.Queens
import Control.Visitor.Worker
import Control.Visitor.Workload
-- }}}

main = defaultMain
    [bench "list of Sum" $ nf (getSum . mconcat . nqueensCount) n
    ,bench "visitor" $ nf (getSum . runVisitor . nqueensCount) n
    ,bench "visitor w/ checkpointing" $ nf (getSum . runVisitorThroughCheckpoint Unexplored . nqueensCount) n
    ,bench "visitor using worker" $ do
        result_mvar ← newEmptyMVar
        _ ← forkWorkerThread
            (putMVar result_mvar)
            (nqueensCount n)
            entire_workload
        _ ← takeMVar result_mvar
        return ()
    ]
  where n = 11