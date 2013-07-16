{-# LANGUAGE UnicodeSyntax #-}

import Control.Applicative ((<$>),(<*>))

import qualified Data.Foldable as Fold
import Data.List (sort)
import qualified Data.Sequence as Seq

import System.Console.CmdTheLine

import Visitor.Checkpoint (Progress(..))
import Visitor.Parallel.Main
import Visitor.Parallel.Adapter.Threads

import Visitor.Examples.Queens

main =
    mainForExploreTreeUntilFoundUsingPush
        (\(_,number_of_solutions) solutions →
            if Seq.length solutions >= number_of_solutions
                then Just solutions
                else Nothing
        )
        driver
        ((,) <$> makeBoardSizeTermAtPosition 0
             <*> required (pos 1 Nothing (posInfo { posName = "SOLUTIONS", posDoc = "number of solutions" }))
        )
        (defTI { termDoc = "print the requested number of n-queens solutions (or at least as many as found) for a given board size" })
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
                Completed (Right (Progress _ solutions)) →
                    Fold.mapM_ print . Seq.unstableSort $ solutions
                Failure message → error $ "error: " ++ message
        )
        (fmap (Seq.singleton . sort) . nqueensSolutions . fst)

