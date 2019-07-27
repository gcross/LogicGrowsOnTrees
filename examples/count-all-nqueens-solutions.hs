{-# LANGUAGE UnicodeSyntax #-}

import Options.Applicative (argument,auto,fullDesc,help,metavar,progDesc)

import LogicGrowsOnTrees.Parallel.Main
import LogicGrowsOnTrees.Parallel.Adapter.Threads
import LogicGrowsOnTrees.Utils.WordSum

import LogicGrowsOnTrees.Examples.Queens

main :: IO ()
main =
    mainForExploreTree
        driver
        (argument auto $ mconcat
            [ help "size of the board"
            , metavar "SIZE"
            ]
        )
        (fullDesc <> progDesc "count the number of n-queens solutions for a given board size")
        (\_ (RunOutcome _ termination_reason) → do
            case termination_reason of
                Aborted _ → error "search aborted"
                Completed (WordSum count) → print count
                Failure _ message → error $ "error: " ++ message
        )
        nqueensCount
