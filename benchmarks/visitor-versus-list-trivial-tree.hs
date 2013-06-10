-- Language extensions {{{
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

-- Imports {{{
import Criterion.Main
import Data.Monoid

import Visitor
import Visitor.Checkpoint
import Visitor.Examples.Tree
import Visitor.Utils.WordSum
import qualified Visitor.Parallel.Common.Worker as Worker
-- }}}

main = defaultMain
    [bench "list" $ nf (getWordSum . mconcat . trivialTree 2) depth
    ,bench "visitor" $ nf (getWordSum . runVisitor . trivialTree 2) depth
    ,bench "visitor w/ checkpointing" $ nf (getWordSum . runVisitorThroughCheckpoint Unexplored . trivialTree 2) depth
    ,bench "visitor using worker" $ Worker.runVisitor (trivialTree 2 depth)
    ]
  where depth = 15
