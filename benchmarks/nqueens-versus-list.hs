-- Language extensions {{{
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

-- Imports {{{
import Control.Concurrent.MVar
import Control.Monad
import Criterion.Main
import Data.Monoid

import Control.Monad.Trans.Visitor
import Control.Monad.Trans.Visitor.Checkpoint
import Control.Monad.Trans.Visitor.Examples.Queens
import Control.Monad.Trans.Visitor.Worker
import Control.Monad.Trans.Visitor.Workload
-- }}}

main = defaultMain
    [bench "list of Sum" $ nf (getIntSum . mconcat . nqueensCount) n
    ,bench "visitor" $ nf (getIntSum . runVisitor . nqueensCount) n
    ,bench "visitor w/ checkpointing" $ nf (getIntSum . runVisitorThroughCheckpoint Unexplored . nqueensCount) n
    ,bench "visitor using worker" $ do
        result_mvar ← newEmptyMVar
        _ ← forkVisitorWorkerThread
            (putMVar result_mvar)
            (nqueensCount n)
            entire_workload
        _ ← takeMVar result_mvar
        return ()
    ]
  where n = 11