{-# LANGUAGE UnicodeSyntax #-}

import Control.Applicative ((<$>),(<*>))

import qualified Data.Foldable as Fold
import Data.List (sort)
import qualified Data.Sequence as Seq

import Options.Applicative (argument,auto,fullDesc,help,metavar,progDesc)

import LogicGrowsOnTrees.Checkpoint (Progress(..))
import LogicGrowsOnTrees.Parallel.Main
import LogicGrowsOnTrees.Parallel.Adapter.Threads

import LogicGrowsOnTrees.Examples.Queens

main :: IO ()
main =
    mainForExploreTreeUntilFoundUsingPull
        (\(_,number_of_solutions) → (>= number_of_solutions) . Seq.length)
        driver
        ((,) <$> board_size_parser
             <*> (argument auto $ mconcat
                     [ metavar "SOLUTIONS"
                     , help "number of solutions"
                     ]
                 )
        )
        (fullDesc <> progDesc "print the requested number of n-queens solutions (or at least as many as found) for a given board size")
        (\_ (RunOutcome _ termination_reason) → do
            case termination_reason of
                Aborted _ → error "search aborted"
                Completed (Left solutions) → do
                    case Seq.length solutions of
                        0 → putStrLn "No solutions were found."
                        1 → do putStrLn $ "Only one solution was found:"
                               Fold.mapM_ print $ solutions
                        n → do putStrLn $ "Only " ++ show n ++ " solutions were found:"
                               Fold.mapM_ print $ solutions
                Completed (Right (Progress _ solutions)) → do
                    Fold.mapM_ print . Seq.unstableSort $ solutions
                Failure _ message → error $ "error: " ++ message
        )
        (fmap (Seq.singleton . sort) . nqueensSolutions . fst)

