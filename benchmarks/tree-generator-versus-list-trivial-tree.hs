-- Language extensions {{{
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

-- Imports {{{
import Criterion.Main
import Data.Monoid

import Visitor
import Visitor.Checkpoint
import Visitor.Utils.Tree
import Visitor.Utils.WordSum
import Visitor.Parallel.Common.VisitorMode (VisitorMode(AllMode))
import Visitor.Parallel.Common.Worker (Purity(Pure),visitTreeGeneric)
-- }}}

main = defaultMain
    [bench "list" $ nf (getWordSum . mconcat . trivialTree 2) depth
    ,bench "tree generator" $ nf (getWordSum . visitTree . trivialTree 2) depth
    ,bench "tree generator w/ checkpointing" $ nf (getWordSum . visitTreeStartingFromCheckpoint Unexplored . trivialTree 2) depth
    ,bench "tree generator using worker" $ visitTreeGeneric AllMode Pure (trivialTree 2 depth)
    ]
  where depth = 15
