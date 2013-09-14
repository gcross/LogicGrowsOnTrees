import GHC.Conc (setNumCapabilities)

import LogicGrowsOnTrees.Parallel.Adapter.Threads
    (RunOutcome(..)
    ,TerminationReason(..)
    ,exploreTreeUntilFirst
    ,setNumberOfWorkers
    )
import LogicGrowsOnTrees.Checkpoint (Progress(..))
import LogicGrowsOnTrees.Examples.Queens (nqueensUsingBitsSolutions)

main = do
    setNumCapabilities 2
    RunOutcome statistics termination_reason <-
        exploreTreeUntilFirst (setNumberOfWorkers 2)
        .
        nqueensUsingBitsSolutions
        $
        10
    case termination_reason of
        Aborted _ -> putStrLn "Search aborted."
        Completed Nothing -> putStrLn "No result found."
        Completed (Just (Progress checkpoint result)) -> putStrLn $ "Found " ++ show result
        Failure _ message -> putStrLn $ "Failed: " ++ message