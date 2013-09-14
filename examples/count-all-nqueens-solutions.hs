{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

import Data.Functor

import System.Console.CmdTheLine

import LogicGrowsOnTrees.Parallel.Adapter.Processes
import LogicGrowsOnTrees.Parallel.Main
import LogicGrowsOnTrees.Utils.WordSum

import LogicGrowsOnTrees.Examples.Queens

main =
    mainForExploreTree
        driver
        (getBoardSize <$> required (flip (pos 0) (posInfo
            {   posName = "BOARD_SIZE"
            ,   posDoc = "board size"
            }
        ) Nothing))
        (defTI { termDoc = "count the number of n-queens solutions for a given board size" })
        (\_ (RunOutcome _ termination_reason) → do
            case termination_reason of
                Aborted _ → error "search aborted"
                Completed (WordSum count) → print count
                Failure _ message → error $ "error: " ++ message
        )
        nqueensCount
