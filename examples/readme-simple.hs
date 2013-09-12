import Control.Monad
import qualified Data.IntSet as IntSet

import LogicGrowsOnTrees
import LogicGrowsOnTrees.Parallel.Main
import LogicGrowsOnTrees.Parallel.Adapter.Threads
import LogicGrowsOnTrees.Utils.Word_
import LogicGrowsOnTrees.Utils.WordSum

-- Code that counts all the solutions for a given input board size. 
nqueensCount 0 = error "board size must be positive"
nqueensCount n =
    -- Start with...
    go n -- ...n queens left...
       0 -- ... at row zero...
       -- ... with all columns available ...
       (IntSet.fromDistinctAscList [0..fromIntegral n-1])
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

main =
    -- Explore the tree generated (implicitly) by nqueensCount in parallel.
    simpleMainForExploreTree
        -- Use threads for parallelism.
        driver

        -- Function that processes the result of the run.
        (\(RunOutcome _ termination_reason) -> do
            case termination_reason of
                Aborted _ -> error "search aborted"
                Completed (WordSum count) -> putStrLn $ "found " ++ show count ++ " solutions"
                Failure _ message -> error $ "error: " ++ message
        )

        -- The logic program that generates the tree to explore.
        (nqueensCount 10)
