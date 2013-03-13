-- Language extensions {{{
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

-- Imports {{{
import Data.Functor
import Data.Serialize (Serialize(..))

import System.Console.CmdTheLine
import System.Environment

import Control.Visitor.Parallel.Main
import Control.Visitor.Parallel.BackEnd.Threads
import Control.Visitor.Utils.WordSum

import Control.Visitor.Examples.Queens
-- }}}

main =
    mainVisitor
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
