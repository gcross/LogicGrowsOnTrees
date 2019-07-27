{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

import Options.Applicative (fullDesc, progDesc)

import LogicGrowsOnTrees.Parallel.Main
import LogicGrowsOnTrees.Parallel.Adapter.Threads
import LogicGrowsOnTrees.Utils.PerfectTree
import LogicGrowsOnTrees.Utils.WordSum



main :: IO ()
main =
    mainForExploreTree
        driver
        arity_and_depth_parser
        (fullDesc <> progDesc "count the leaves of a tree")
        (\_ (RunOutcome _ termination_reason) → do
            case termination_reason of
                Aborted _ → error "search aborted"
                Completed (WordSum count) → print count
                Failure _ message → error $ "error: " ++ message
        )
        (trivialPerfectTree <$> arity <*> depth)
