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
import Control.Monad.Trans.Visitor.Worker
import Control.Monad.Trans.Visitor.Workload
-- }}}

sumtree :: MonadPlus m ⇒ Int → m (Sum Int)
sumtree 0 = return (Sum 1)
sumtree n = sumtree (n-1) `mplus` sumtree (n-1)
{-# SPECIALIZE sumtree :: Int → [Sum Int] #-}
{-# SPECIALIZE sumtree :: Int → Visitor (Sum Int) #-}

main = defaultMain
    [bench "list" $ nf (getSum . mconcat . sumtree) depth
    ,bench "visitor" $ nf (getSum . runVisitor . sumtree) depth
    ,bench "visitor w/ checkpointing" $ nf (getSum . runVisitorThroughCheckpoint Unexplored . sumtree) depth
    ,bench "visitor using worker" $ do
        result_mvar ← newEmptyMVar
        _ ← forkVisitorWorkerThread
            (putMVar result_mvar)
            (sumtree depth)
            entire_workload
        _ ← takeMVar result_mvar
        return ()
    ]
  where depth = 19