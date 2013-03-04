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
import qualified Control.Visitor.Worker as Worker
import Control.Visitor.Workload
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
    ,bench "visitor using worker" $ Worker.runVisitor (sumtree depth)
    ]
  where depth = 12