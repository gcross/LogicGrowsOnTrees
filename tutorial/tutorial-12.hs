import System.Console.CmdTheLine (PosInfo(..),TermInfo(..),defTI,pos,posInfo,required)

import LogicGrowsOnTrees.Parallel.Adapter.Threads (driver)
import LogicGrowsOnTrees.Parallel.Main (RunOutcome(..),TerminationReason(..),mainForExploreTree)
import LogicGrowsOnTrees.Utils.WordSum (WordSum(..))

import LogicGrowsOnTrees.Examples.Queens (nqueensUsingBitsSolutions)

main =
    mainForExploreTree
        driver
        (required $
            pos 0
                (Nothing :: Maybe Int)
                posInfo
                  { posName = "BOARD_SIZE"
                  , posDoc = "the size of the board"
                  }
        )
        (defTI
            { termName = "tutorial-11"
            , termDoc = "count the number of n-queens solutions for a given board size"
            }
        )
        (\board_size (RunOutcome _ termination_reason) -> do
            case termination_reason of
                Aborted _ -> error "search aborted"
                Completed (WordSum count) -> putStrLn $ show count ++ " solutions found for board size " ++ show board_size
                Failure _ message -> error $ "error: " ++ message
        )
        (fmap (const $ WordSum 1) . nqueensUsingBitsSolutions . fromIntegral)
