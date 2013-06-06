-- Language extensions {{{
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

-- Imports {{{
import Criterion.Main
import Data.Monoid

import Control.Visitor
import Control.Visitor.Checkpoint
import Control.Visitor.Examples.Queens
import Control.Visitor.Utils.WordSum
import qualified Control.Visitor.Parallel.Common.Worker as Worker
-- }}}

main = defaultMain
    [bench "list of Sum" $ nf (getWordSum . mconcat . nqueensCount) n
    ,bench "visitor" $ nf (getWordSum . runVisitor . nqueensCount) n
    ,bench "visitor w/ checkpointing" $ nf (getWordSum . runVisitorThroughCheckpoint Unexplored . nqueensCount) n
    ,bench "visitor using worker" $ Worker.runVisitor (nqueensCount n)
    ]
  where n = 13
