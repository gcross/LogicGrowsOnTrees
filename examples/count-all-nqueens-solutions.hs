{-# LANGUAGE UnicodeSyntax #-}

import System.Console.CmdTheLine

import LogicGrowsOnTrees.Parallel.Main
import LogicGrowsOnTrees.Parallel.Adapter.Threads
import LogicGrowsOnTrees.Utils.WordSum

import LogicGrowsOnTrees.Examples.Queens

main =
    mainForExploreTree
        driver
        (makeBoardSizeTermAtPosition 0)
        (defTI { termDoc = "count the number of n-queens solutions for a given board size" })
        (\_ (RunOutcome _ termination_reason) → do
            case termination_reason of
                Aborted _ → error "search aborted"
                Completed (WordSum count) → print count
                Failure message → error $ "error: " ++ message
        )
        nqueensCount
