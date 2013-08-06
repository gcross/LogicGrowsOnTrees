import Control.Monad (void)
import GHC.Conc (setNumCapabilities)

import LogicGrowsOnTrees.Parallel.Adapter.Threads
    (RunOutcome(..)
    ,TerminationReason(..)
    ,changeNumberOfWorkers
    ,exploreTreeUntilFoundUsingPush
    )
import LogicGrowsOnTrees.Checkpoint (Progress(..))
import LogicGrowsOnTrees.Examples.Queens (nqueensUsingBitsSolutions)

main = do
    setNumCapabilities 2
    RunOutcome statistics termination_reason <-
        exploreTreeUntilFoundUsingPush
            ((>= 5) . length)
            (void . changeNumberOfWorkers . const . return $ 2)
        .
        fmap (:[])
        .
        nqueensUsingBitsSolutions
        $
        10
    case termination_reason of
        Aborted _ -> putStrLn "Search aborted."
        Completed (Left results) -> putStrLn $ "Only found:" ++ show results
        Completed (Right (Progress checkpoint results)) -> putStrLn $ "Found: " ++ show results
        Failure _ message -> putStrLn $ "Failed: " ++ message