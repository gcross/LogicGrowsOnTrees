{-# LANGUAGE UnicodeSyntax #-}

import Data.List (sort)
import Options.Applicative (fullDesc,progDesc)

import LogicGrowsOnTrees.Checkpoint (Progress(..))
import LogicGrowsOnTrees.Parallel.Main
import LogicGrowsOnTrees.Parallel.Adapter.Threads

import LogicGrowsOnTrees.Examples.Queens

main :: IO ()
main =
    mainForExploreTreeUntilFirst
        driver
        boardSizeArgument
        (fullDesc <> progDesc "print all the n-queens solutions for a given board size")
        (\_ (RunOutcome _ termination_reason) → do
            case termination_reason of
                Aborted _ → error "search aborted"
                Completed Nothing → putStrLn "No solution found."
                Completed (Just (Progress _ result)) → print (sort result)
                Failure _ message → error $ "error: " ++ message
        )
        nqueensSolutions

