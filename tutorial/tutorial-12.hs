import Control.Applicative (liftA2)
import System.Console.CmdTheLine (PosInfo(..),TermInfo(..),defTI,pos,posInfo,required)

import LogicGrowsOnTrees.Checkpoint (Progress(..))
import LogicGrowsOnTrees.Parallel.Adapter.Threads (driver)
import LogicGrowsOnTrees.Parallel.Main (RunOutcome(..),TerminationReason(..),mainForExploreTreeUntilFoundUsingPush)
import LogicGrowsOnTrees.Utils.WordSum (WordSum(..))

import LogicGrowsOnTrees.Examples.Queens (nqueensUsingBitsSolutions)

main =
    mainForExploreTreeUntilFoundUsingPush
        (\(board_size,number_to_find) -> (>= number_to_find) . length)
        driver
        (liftA2 (,)
            (required $
                pos 0
                    (Nothing :: Maybe Int)
                    posInfo
                      { posName = "BOARD_SIZE"
                      , posDoc = "the size of the board"
                      }
            )
            (required $
                pos 1
                    (Nothing :: Maybe Int)
                    posInfo
                      { posName = "#"
                      , posDoc = "the number of solutions to find"
                      }
            )
        )
        (defTI
            { termName = "tutorial-12"
            , termDoc = "find some of the solutions to the n-queens problem for a given board size"
            }
        )
        (\(board_size,number_to_find) (RunOutcome _ termination_reason) -> do
            case termination_reason of
                Aborted _ -> error "search aborted"
                Completed (Left found) -> do
                    putStrLn $ "For board size " ++ show board_size ++ ", only found " ++ show (length found) ++ "/" ++ show number_to_find ++ " solutions:"
                    mapM_ print found
                Completed (Right (Progress checkpoint found)) -> do
                    putStrLn $ "Found all " ++ show number_to_find ++ " requested solutions for board size " ++ show board_size ++ ":"
                    mapM_ print found
                Failure _ message -> error $ "error: " ++ message
        )
        (fmap (:[]) . nqueensUsingBitsSolutions . fromIntegral . fst)
