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
import Control.Visitor.Utils.WordSum
import Control.Visitor.Worker
import Control.Visitor.Workload
-- }}}

main = defaultMain
    [bench "list of Sum" $ nf (getWordSum . mconcat . nqueensCount) n
    ,bench "visitor" $ nf (getWordSum . runVisitor . nqueensCount) n
    ,bench "visitor w/ checkpointing" $ nf (getWordSum . runVisitorThroughCheckpoint Unexplored . nqueensCount) n
    ,bench "visitor using worker" $ do
        result_mvar ← newEmptyMVar
        _ ← forkVisitorWorkerThread
            (putMVar result_mvar)
            (nqueensCount n)
            entire_workload
        _ ← takeMVar result_mvar
        return ()
    ]
  where n = 14