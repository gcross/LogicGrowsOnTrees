import GHC.Conc (setNumCapabilities)

import LogicGrowsOnTrees.Parallel.Adapter.Threads
    (RunOutcome(..)
    ,TerminationReason(..)
    ,changeNumberOfWorkers
    ,exploreTree
    ,setNumberOfWorkers
    )
import LogicGrowsOnTrees.Utils.WordSum (WordSum(..))
import LogicGrowsOnTrees.Examples.Queens (nqueensUsingBitsSolutions)

main = do
    setNumCapabilities 2
    RunOutcome statistics termination_reason <-
        exploreTree (setNumberOfWorkers 2)
        .
        fmap (const $ WordSum 1)
        .
        nqueensUsingBitsSolutions
        $
        10
    case termination_reason of
        Aborted progress -> putStrLn "Count aborted."
        Completed (WordSum count) -> putStrLn $ "Found " ++ show count ++ " solutions."
        Failure progress message -> putStrLn $ "Failed: " ++ message