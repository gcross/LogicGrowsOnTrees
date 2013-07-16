{-# LANGUAGE UnicodeSyntax #-}

import System.Console.CmdTheLine

import Visitor.Parallel.Main
import Visitor.Parallel.Adapter.Threads
import Visitor.Utils.WordSum

import Visitor.Examples.Queens

main =
    mainForVisitTree
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
