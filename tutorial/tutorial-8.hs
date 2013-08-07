import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import System.IO (hFlush,stdout)

import LogicGrowsOnTrees.Parallel.Adapter.Threads
    (RunOutcome(..)
    ,TerminationReason(..)
    ,changeNumberOfWorkers
    ,exploreTree
    )
import LogicGrowsOnTrees.Utils.WordSum (WordSum(..))
import LogicGrowsOnTrees.Examples.Queens (nqueensUsingBitsSolutions)

main = do
    RunOutcome _ termination_reason <-
        exploreTree (forever $
            liftIO (do
                putStr "Enter the desired number of workers: "
                hFlush stdout
                readLn
            )
            >>=
            changeNumberOfWorkers . const . return
            >>=
            liftIO . putStrLn . (\n -> "Now there are " ++ show n ++ " workers.")
        )
        .
        fmap (const $ WordSum 1)
        .
        nqueensUsingBitsSolutions
        $
        14
    case termination_reason of
        Aborted progress -> putStrLn "Count aborted."
        Completed (WordSum count) -> putStrLn $ "Found " ++ show count ++ " solutions."
        Failure _ message -> putStrLn $ "Failed: " ++ message