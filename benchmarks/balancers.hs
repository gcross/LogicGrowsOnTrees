-- Language extensions {{{
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

-- Imports {{{
import Criterion.Main

import Visitor
import Visitor.Utils.WordSum
-- }}}

main = defaultMain
    [bgroup "allFrom" $ benchUsing allFrom
    ,bgroup "allFromBalanced" $ benchUsing allFromBalanced
    ,bgroup "allFromBalancedGreedy" $ benchUsing allFromBalancedGreedy
    ]
  where
    benchUsing f =
        [ bench (show bound) $ nf (getWordSum . visitTree . f) (map WordSum [1..bound])
        | bound ← [1,10,100,1000]
        ]
