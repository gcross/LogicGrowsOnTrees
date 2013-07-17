{-# LANGUAGE UnicodeSyntax #-}

import System.Console.CmdTheLine

import qualified Data.Foldable as Fold
import Data.List (sort)
import qualified Data.Sequence as Seq

import LogicGrowsOnTrees.Parallel.Main
import LogicGrowsOnTrees.Parallel.Adapter.Threads

import LogicGrowsOnTrees.Examples.Queens

main =
    mainForExploreTree
        driver
        (makeBoardSizeTermAtPosition 0)
        (defTI { termDoc = "print all the n-queens solutions for a given board size" })
        (\_ (RunOutcome _ termination_reason) → do
            case termination_reason of
                Aborted _ → error "search aborted"
                Completed solutions → Fold.mapM_ print . Seq.unstableSort $ solutions
                Failure message → error $ "error: " ++ message
        )
        (fmap (Seq.singleton . sort) . nqueensSolutions)

