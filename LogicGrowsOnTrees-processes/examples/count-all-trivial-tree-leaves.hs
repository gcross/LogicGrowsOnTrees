{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

import Control.Applicative

import System.Console.CmdTheLine

import LogicGrowsOnTrees.Parallel.Main
import LogicGrowsOnTrees.Parallel.Adapter.Processes
import LogicGrowsOnTrees.Utils.PerfectTree
import LogicGrowsOnTrees.Utils.WordSum

main =
    mainForExploreTree
        driver
        (makeArityAndDepthTermAtPositions 0 1)
        (defTI { termDoc = "count the leaves of a tree" })
        (\_ (RunOutcome _ termination_reason) → do
            case termination_reason of
                Aborted _ → error "search aborted"
                Completed (WordSum count) → print count
                Failure _ message → error $ "error: " ++ message
        )
        (trivialPerfectTree <$> arity <*> depth)
