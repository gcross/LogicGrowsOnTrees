-- Language extensions {{{
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

-- Imports {{{
import Criterion.Main

import LogicGrowsOnTrees
import LogicGrowsOnTrees.Utils.WordSum
-- }}}

main = defaultMain
    [bgroup "allFrom" $ benchUsing allFrom
    ,bgroup "allFromBalanced" $ benchUsing allFromBalanced
    ,bgroup "allFromBalancedBottomUp" $ benchUsing allFromBalancedBottomUp
    ]
  where
    benchUsing f =
        [ bench (show bound) $ nf (getWordSum . exploreTree . f) (map WordSum [1..bound])
        | bound ← [1,10,100,1000]
        ]
