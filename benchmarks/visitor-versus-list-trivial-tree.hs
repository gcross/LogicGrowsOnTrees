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
import Control.Visitor.Examples.Tree
import Control.Visitor.Utils.WordSum
import qualified Control.Visitor.Worker as Worker
import Control.Visitor.Workload
-- }}}

main = defaultMain
    [bench "list" $ nf (getWordSum . mconcat . trivialTree 2) depth
    ,bench "visitor" $ nf (getWordSum . runVisitor . trivialTree 2) depth
    ,bench "visitor w/ checkpointing" $ nf (getWordSum . runVisitorThroughCheckpoint Unexplored . trivialTree 2) depth
    ,bench "visitor using worker" $ Worker.runVisitor (trivialTree 2 depth)
    ]
  where depth = 15