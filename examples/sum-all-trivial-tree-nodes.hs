-- Language extensions {{{
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

-- Imports {{{
import Control.Applicative

import Data.Composition ((.*))
import Data.Serialize (Serialize(..))

import System.Console.CmdTheLine
import System.Environment

import Control.Visitor.Main
import Control.Visitor.Parallel.Threads
import Control.Visitor.Utils.WordSum

import Control.Visitor.Examples.RoseTree
import Control.Visitor.Visitors.RoseTree
-- }}}

main =
    mainVisitor
        driver
        (makeArityAndDepthTermAtPositions 0 1)
        (defTI { termDoc = "sum the unit nodes of a trivial tree" })
        (\_ (RunOutcome _ termination_reason) → do
            case termination_reason of
                Aborted _ → error "search aborted"
                Completed (WordSum count) → print count
                Failure message → error $ "error: " ++ message
        )
        (sumOverAllNodes . (generateTrivialTree <$> arity <*> depth))
