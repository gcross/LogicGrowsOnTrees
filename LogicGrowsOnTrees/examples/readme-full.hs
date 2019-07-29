import Control.Monad (guard)
import qualified Data.IntSet as IntSet
import Options.Applicative (argument, auto, fullDesc, help, metavar, progDesc)
import Text.Printf (printf)

import LogicGrowsOnTrees (Tree, allFrom)
import LogicGrowsOnTrees.Parallel.Main
  ( RunOutcome(..)
  , TerminationReason(..)
  , mainForExploreTree
  )
import LogicGrowsOnTrees.Parallel.Adapter.Threads (driver)
import LogicGrowsOnTrees.Utils.WordSum (WordSum(..))

-- Code that counts all the solutions for a given input board size.
nqueensCount :: Word -> Tree WordSum
nqueensCount 0 = error "board size must be positive"
nqueensCount board_size =
    -- Start with...
    go board_size -- ...n queens left...
       0 -- ... at row zero...
       -- ... with all columns available ...
       (IntSet.fromDistinctAscList [0..fromIntegral board_size-1])
       IntSet.empty -- ... with no occupied negative diagonals...
       IntSet.empty -- ... with no occupied positive diagonals.
  where
    -- We have placed the last queen, so this is a solution!
    go 0 _ _ _ _ = return (WordSum 1)

    -- We are still placing queens.
    go n
       row
       available_columns
       occupied_negative_diagonals
       occupied_positive_diagonals
     = do
        -- Pick one of the available columns.
        column <- allFrom $ IntSet.toList available_columns

        -- See if this spot conflicts with another queen on the negative diagonal.
        let negative_diagonal = row + column
        guard $ IntSet.notMember negative_diagonal occupied_negative_diagonals

        -- See if this spot conflicts with another queen on the positive diagonal.
        let positive_diagonal = row - column
        guard $ IntSet.notMember positive_diagonal occupied_positive_diagonals

        -- This spot is good!  Place a queen here and move on to the next row.
        go (n-1)
           (row+1)
           (IntSet.delete column available_columns)
           (IntSet.insert negative_diagonal occupied_negative_diagonals)
           (IntSet.insert positive_diagonal occupied_positive_diagonals)

main :: IO ()
main =
    -- Explore the tree generated (implicitly) by nqueensCount in parallel.
    mainForExploreTree
        -- Use threads for parallelism.
        driver

        -- Use a single positional required command-line argument to get the board size.
        (argument auto $ mconcat
            [ metavar "BOARD_SIZE"
            , help "the size of the board"
            ]
        )

        -- Information about the program (for the help screen).
        (mconcat
            [ fullDesc
            , progDesc "count the number of n-queens solutions for a given board size"
            ]
        )

        -- Function that processes the result of the run.
        (\n (RunOutcome _ termination_reason) -> putStrLn $
            case termination_reason of
                Aborted _ -> "Search aborted."
                Completed (WordSum count) ->
                    printf "For a size %i board, found %i solutions." n count
                Failure _ message -> "Error: " ++ message
        )

        -- The logic program that generates the tree to explore.
        nqueensCount
