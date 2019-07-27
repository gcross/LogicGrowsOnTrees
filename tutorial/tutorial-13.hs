import Control.Applicative (liftA2)
import Options.Applicative (argument, auto, fullDesc, help, metavar, progDesc)

import LogicGrowsOnTrees.Checkpoint (Progress(..))
import LogicGrowsOnTrees.Parallel.Adapter.Threads (driver)
import LogicGrowsOnTrees.Parallel.Main (RunOutcome(..),TerminationReason(..),mainForExploreTreeUntilFoundUsingPush)

import LogicGrowsOnTrees.Examples.Queens (board_size_parser, nqueensUsingBitsSolutions)

main :: IO ()
main =
    mainForExploreTreeUntilFoundUsingPush
        (\(_,number_to_find) -> (>= number_to_find) . length)
        driver
        (liftA2 (,)
            board_size_parser
            (argument auto $ mconcat
                [ metavar "#"
                , help "the number of solutions to find"
                ]
            )
        )
        (fullDesc <> progDesc
            "tutorial-12 - find some of the solutions to the n-queens problem for a given board size"
        )
        (\(board_size,number_to_find) (RunOutcome _ termination_reason) -> do
            case termination_reason of
                Aborted _ -> error "search aborted"
                Completed (Left found) -> do
                    putStrLn $ "For board size " ++ show board_size ++ ", only found " ++ show (length found) ++ "/" ++ show number_to_find ++ " solutions:"
                    mapM_ print found
                Completed (Right (Progress _ found)) -> do
                    putStrLn $ "Found all " ++ show number_to_find ++ " requested solutions for board size " ++ show board_size ++ ":"
                    mapM_ print found
                Failure _ message -> error $ "error: " ++ message
        )
        (fmap (:[]) . nqueensUsingBitsSolutions . fromIntegral . fst)
