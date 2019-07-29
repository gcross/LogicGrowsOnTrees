import Control.Monad (guard)
import qualified Data.IntSet as IntSet
import Options.Applicative (fullDesc, header, progDesc)
import Text.Printf (printf)

import LogicGrowsOnTrees (Tree, allFrom)
import LogicGrowsOnTrees.Parallel.Main
  ( RunOutcome(..)
  , TerminationReason(..)
  , simpleMainForExploreTree
  )
import LogicGrowsOnTrees.Parallel.Adapter.Threads (driver)
import LogicGrowsOnTrees.Utils.WordSum (WordSum(..))

-- Code that counts all the solutions for a given input board size.
nqueensCount :: Int -> Tree WordSum
nqueensCount 0 = error "board size must be positive"
nqueensCount board_size =
    -- Start with...
    go board_size -- ...board_size queens left...
       0 -- ... at row zero...
       (IntSet.fromDistinctAscList [0..fromIntegral board_size-1])
         -- ... with all columns available ...
       IntSet.empty -- ... with no occupied negative diagonals...
       IntSet.empty -- ... with no occupied positive diagonals.
  where
    -- We have placed the last queen, so this is a solution!
    go 0 _ _ _ _ = pure (WordSum 1)

    -- We are still placing queens.
    go number_of_queens_left
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
        go (number_of_queens_left-1)
           (row+1)
           (IntSet.delete column available_columns)
           (IntSet.insert negative_diagonal occupied_negative_diagonals)
           (IntSet.insert positive_diagonal occupied_positive_diagonals)

main :: IO ()
main =
    -- Explore the tree generated (implicitly) by nqueensCount in parallel.
    simpleMainForExploreTree
        -- Use threads for parallelism.
        driver

        -- Program description.
        (mconcat
            [ fullDesc
            , progDesc "Count the number of solutions to the N-queens problem on a 10x10 board."
            , header "readme-simple - a program that solves the N-queens problem"
            ])

        -- Function that processes the result of the run.
        (\(RunOutcome _ termination_reason) -> putStrLn $
            case termination_reason of
                Aborted _ -> "Search aborted."
                Completed (WordSum count) ->
                    printf "For a 10x10 board, found %i solutions." count
                Failure _ message -> "Error: " ++ message
        )

        -- The logic program that generates the tree to explore.
        (nqueensCount 10)
