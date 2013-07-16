-- Language extensions {{{
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

-- Imports {{{
import Control.Applicative

import System.Console.CmdTheLine

import Visitor.Parallel.Main
import Visitor.Parallel.Adapter.Threads
import Visitor.Utils.PerfectTree
import Visitor.Utils.WordSum

-- }}}

main =
    mainForVisitTree
        driver
        (makeArityAndDepthTermAtPositions 0 1)
        (defTI { termDoc = "count the leaves of a tree" })
        (\_ (RunOutcome _ termination_reason) → do
            case termination_reason of
                Aborted _ → error "search aborted"
                Completed (WordSum count) → print count
                Failure message → error $ "error: " ++ message
        )
        (trivialPerfectTree <$> arity <*> depth)
