{-# LANGUAGE UnicodeSyntax #-}

import qualified Data.Foldable as Fold
import Data.List (sort)
import qualified Data.Sequence as Seq
import Options.Applicative (fullDesc,progDesc)

import LogicGrowsOnTrees.Parallel.Main
import LogicGrowsOnTrees.Parallel.Adapter.Threads

import LogicGrowsOnTrees.Examples.Queens

main :: IO ()
main =
    mainForExploreTree
        driver
        board_size_parser
        (fullDesc <> progDesc "print all the n-queens solutions for a given board size")
        (\_ (RunOutcome _ termination_reason) → do
            case termination_reason of
                Aborted _ → error "search aborted"
                Completed solutions → Fold.mapM_ print . Seq.unstableSort $ solutions
                Failure _ message → error $ "error: " ++ message
        )
        (fmap (Seq.singleton . sort) . nqueensSolutions)

