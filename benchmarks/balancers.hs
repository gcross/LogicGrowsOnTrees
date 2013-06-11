-- Language extensions {{{
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

-- Imports {{{
import Criterion.Main

import Visitor
import Visitor.Utils.WordSum
-- }}}

main = defaultMain
    [bench "allFrom" $ nf (getWordSum . visitTree . allFrom) inputs
    ,bench "allFromBalanced" $ nf (getWordSum . visitTree . allFromBalanced) inputs
    ,bench "allFromBalancedGreedy" $ nf (getWordSum . visitTree . allFromBalancedGreedy) inputs
    ]
  where inputs = map WordSum [1..1000]
