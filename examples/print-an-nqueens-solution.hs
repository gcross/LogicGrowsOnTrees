{-# LANGUAGE UnicodeSyntax #-}

import Data.List (sort)

import System.Console.CmdTheLine

import Visitor.Checkpoint (Progress(..))
import Visitor.Parallel.Main
import Visitor.Parallel.BackEnd.Threads

import Visitor.Examples.Queens

main =
    mainForVisitTreeUntilFirst
        driver
        (makeBoardSizeTermAtPosition 0)
        (defTI { termDoc = "print an n-queens solutions for a given board size" })
        (\_ (RunOutcome _ termination_reason) → do
            case termination_reason of
                Aborted _ → error "search aborted"
                Completed Nothing → putStrLn "No solution found."
                Completed (Just (Progress _ result)) → print (sort result)
                Failure message → error $ "error: " ++ message
        )
        nqueensSolutions

