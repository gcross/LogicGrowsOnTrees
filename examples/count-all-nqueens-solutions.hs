-- Language extensions {{{
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

-- Imports {{{
import Data.Functor
import Data.Serialize (Serialize(..))

import System.Console.CmdTheLine
import System.Environment

import Control.Visitor.Main
import Control.Visitor.Parallel.Threads
import Control.Visitor.Utils.IntSum

import Control.Visitor.Examples.Queens
-- }}}

main =
    mainVisitor
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
                Completed (IntSum count) → print count
                Failure message → error $ "error: " ++ message
        )
        nqueensCount
