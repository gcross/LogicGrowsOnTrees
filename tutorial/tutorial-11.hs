import LogicGrowsOnTrees.Parallel.Adapter.Threads (driver)
import LogicGrowsOnTrees.Parallel.Main (RunOutcome(..),TerminationReason(..),simpleMainForExploreTree)
import LogicGrowsOnTrees.Utils.WordSum (WordSum(..))

import LogicGrowsOnTrees.Examples.Queens (nqueensUsingBitsSolutions)

main =
    simpleMainForExploreTree
        driver
        (\(RunOutcome _ termination_reason) -> do
            case termination_reason of
                Aborted _ -> error "search aborted"
                Completed (WordSum count) -> putStrLn $ show count ++ " solutions were found"
                Failure _ message -> error $ "error: " ++ message
        )
        (fmap (const $ WordSum 1) (nqueensUsingBitsSolutions 10))
