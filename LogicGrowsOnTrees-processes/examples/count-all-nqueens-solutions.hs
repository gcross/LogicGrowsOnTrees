{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

import Options.Applicative (fullDesc,progDesc)

import LogicGrowsOnTrees.Parallel.Adapter.Processes
import LogicGrowsOnTrees.Parallel.Main
import LogicGrowsOnTrees.Utils.WordSum

import LogicGrowsOnTrees.Examples.Queens

main :: IO ()
main =
    mainForExploreTree
        driver
        board_size_parser
        (fullDesc <> progDesc "count the number of n-queens solutions for a given board size")
        (\_ (RunOutcome _ termination_reason) →
            case termination_reason of
                Aborted _ → error "search aborted"
                Completed (WordSum count) → print count
                Failure _ message → error $ "error: " ++ message
        )
        nqueensCount
