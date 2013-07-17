-- Language extensions {{{
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

-- Imports {{{
import Control.Applicative
import Control.Arrow ((***))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

import Data.Bits
import Data.List
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Word

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit

import Text.Printf

import LogicGrowsOnTrees
import LogicGrowsOnTrees.Examples.Queens
import LogicGrowsOnTrees.Examples.Queens.Advanced
import LogicGrowsOnTrees.Utils.WordSum
-- }}}

-- Functions {{{
checkBlocks :: [(Word,Word)] → Int → Int → Word64 → Word64 → Word64 → Word64 → Assertion -- {{{
checkBlocks
    solution
    window_start
    window_size
    original_occupied_rows
    original_occupied_columns
    original_occupied_negative_diagonals
    original_occupied_positive_diagonals
 = go
   (map (fromIntegral *** fromIntegral) solution)
   (original_occupied_rows .&. rows_and_columns_mask)
   (original_occupied_columns .&. rows_and_columns_mask)
   (original_occupied_negative_diagonals .&. negative_diagonals_mask)
   (original_occupied_positive_diagonals .&. positive_diagonals_mask)
  where
    rows_and_columns_mask = bit window_size - 1
    negative_diagonals_mask = bit (2*window_size-1) - 1
    positive_diagonals_mask = negative_diagonals_mask `rotateR` (window_size-1) 
    window_end = window_start+window_size-1
    go :: [(Int,Int)] → Word64 → Word64 → Word64 → Word64 → IO ()
    go [] 0 0 0 0 = return ()
    go []
       occupied_rows
       occupied_columns
       occupied_negative_diagonals
       occupied_positive_diagonals
      = assertFailure (
            printf "non-zero blocks %i %i %i %i (from %i %i %i %i) at row %i for solution: %s"
                occupied_rows
                occupied_columns
                occupied_negative_diagonals
                occupied_positive_diagonals
                original_occupied_rows
                original_occupied_columns
                original_occupied_negative_diagonals
                original_occupied_positive_diagonals
                window_start
                (show solution)
        )
    go ((row,col):rest_solution)
       occupied_rows
       occupied_columns
       occupied_negative_diagonals
       occupied_positive_diagonals
       = do
        let row_bit = if row >= window_start && row <= window_end then bit (row-window_start) else 0
        when (row_bit /= 0) $
            assertBool
                (printf "bit for row %i (%i) in %i was not set for solution %s" row (row-window_start) occupied_rows (show solution))
                (row_bit .&. occupied_rows /= 0)
        let col_bit = if col >= window_start && col <= window_end then bit (col-window_start) else 0
        when (col_bit /= 0) $
            assertBool
                (printf "bit for col %i (%i) in %i was not set for solution %s" col (col-window_start) occupied_columns (show solution))
                (col_bit .&. occupied_columns /= 0)
        let neg_bit = if col+row - 2*window_start >= 0 && col+row - 2*window_start <= 2*(window_size-1) then bit (col+row-2*window_start) else 0
        when (neg_bit /= 0) $
            assertBool
                (printf "bit for negative diagonal %i (%i) in %i was not set for solution %s" (col+row) (col+row-2*window_start) occupied_negative_diagonals (show solution))
                (neg_bit .&. occupied_negative_diagonals /= 0)
        let pos_bit = if col-row >= -(window_size-1) && col-row <= (window_size-1) then 1 `rotate` (col-row) else 0
        when (pos_bit /= 0) $
            assertBool
                (printf "bit for positive diagonal %i (%i) in %i was not set for solution %s" (col-row) (col-row) occupied_positive_diagonals (show solution))
                (pos_bit .&. occupied_positive_diagonals /= 0)
        go rest_solution
           (occupied_rows `xor` row_bit)
           (occupied_columns `xor` col_bit)
           (occupied_negative_diagonals `xor` neg_bit)
           (occupied_positive_diagonals `xor` pos_bit)
-- }}}

checkRightPositiveBlocks :: Int → Word64 → Word64 → Assertion -- {{{
checkRightPositiveBlocks size occupied_positive_diagonals occupied_right_positive_diagonals = go 0 1 1
  where
    go column top_bit right_bit
      | column == size = return ()
      | otherwise = do
         assertEqual
            (printf "for %i, column bit (%s in %i) does not match row bit (%s in %i)"
                 column
                (show top_bit_value)
                 occupied_positive_diagonals
                (show right_bit_value)
                 occupied_right_positive_diagonals
            )
            top_bit_value
            right_bit_value
         go (column+1) (top_bit `rotateR` 1) (right_bit `unsafeShiftL` 1)
      where
         top_bit_value = occupied_positive_diagonals .&. top_bit /= 0
         right_bit_value = occupied_right_positive_diagonals .&. right_bit /= 0
-- }}}

checkSolutionIsValid :: Word → NQueensSolution → Assertion -- {{{
checkSolutionIsValid n solution =
    forM_ (zip [0..] solution) $ \(i,(row1,col1)) → do
        assertBool "row within bounds" $ row1 >= 0 && row1 < n
        assertBool "column within bounds" $ col1 >= 0 && col1 < n
        forM_ (drop (i+1) solution) $ \(row2,col2) → do
            assertBool ("rows conflict in " ++ show solution) $ row1 /= row2
            assertBool ("columns conflict in " ++ show solution) $ col1 /= col2
            assertBool ("negative diagonals conflict in " ++ show solution) $ row1+col1 /= row2+col2
            assertBool ("positive diagonals conflict in " ++ show solution) $ row1-col1 /= row2-col2
-- }}}

checkSolutionsAreValid :: Word → NQueensSolutions → Assertion -- {{{
checkSolutionsAreValid = mapM_ . checkSolutionIsValid
-- }}}

checkSymmetry :: MonadIO m ⇒ Word → NQueensSymmetry → [(Word,Word)] → m () -- {{{
checkSymmetry n correct_symmetry solution =
        liftIO
        .
        assertEqual ("solution has wrong symmetry: " ++ show (reverse solution)) correct_symmetry
        .
        symmetryOf n
        $
        solution
-- }}}

remdups :: (Eq a) => [a] -> [a] -- {{{
remdups []  =  []
remdups (x : []) =  [x]
remdups (x : xx : xs)
 | x == xx   = remdups (x : xs)
 | otherwise = x : remdups (xx : xs)
-- }}}

testSolutionsUsing nqueensSolutions nqueensCount = -- {{{
    [testGroup "are valid" $ -- {{{
        map (\n → testCase ("n = " ++ show n) $ checkSolutionsAreValid n (nqueensSolutions n))
            [1..10]
     -- }}}
    ,testGroup "are unique" $ -- {{{
        [ testCase ("n = " ++ show n) $
            let solutions_as_list = nqueensSolutions n
                solutions_as_set = Set.fromList solutions_as_list
            in length solutions_as_list @?= Set.size solutions_as_set
        | n ← [1..10]
        ]
     -- }}}
    ,testGroup "have correct size" -- {{{
        [ testCase ("n = " ++ show n) $
            (correct_count @=?)
            .
            getWordSum
            .
            exploreTree
            .
            nqueensCount
            $
            n
        | n ← [1..14]
        , let correct_count = nqueensCorrectCount n
        ]
     -- }}}
    ,testGroup "match count" -- {{{
        [ testCase ("n = " ++ show n) $
            (nqueensCorrectCount n @=?)
            .
            getWordSum
            .
            exploreTree
            .
            nqueensCount
            $
            n
        | n ← [1..10]
        ]
     -- }}}
    ]
-- }}}
-- }}}

main = defaultMain tests

tests = -- {{{
    [testProperty "reflectBits" $ liftA2 (==) id (reflectBits . reflectBits)
    ,testGroup "reflections and rotations" -- {{{
        [testProperty "reflecting twice = id" $ \n → -- {{{
            liftA2 (==)
                (reflectSolution n . reflectSolution n)
                 id
         -- }}}
        ,testProperty "rotating left twice = rotate 180"$ \n → -- {{{
            liftA2 (==)
                (rotateLeft n . rotateLeft n)
                (rotate180 n)
         -- }}}
        ,testProperty "rotating left four times = id" $ \n → -- {{{
            liftA2 (==)
                (rotateLeft n . rotateLeft n . rotateLeft n . rotateLeft n)
                 id
         -- }}}
        ,testProperty "rotating right twice = rotate 180" $ \n → -- {{{
            liftA2 (==)
                (rotateRight n . rotateRight n)
                (rotate180 n)
         -- }}}
        ,testProperty "rotating right four times = id" $ \n → -- {{{
            liftA2 (==)
                (rotateRight n . rotateRight n . rotateRight n . rotateRight n)
                 id
         -- }}}
        ]
     -- }}}
    ,testGroup "symmetry breaking" -- {{{
        [testGroup "start" $ -- {{{
            let getAllSolutions :: MonadPlus m ⇒ Word → m [(Word,Word)] -- {{{
                getAllSolutions =
                    nqueensStart
                        (++)
                        (const . return)
                        (const . return)
                        (const . const . return)
                        []
            in -- }}}
            [testGroup "correct blocks" -- {{{
                [ testCase ("n = " ++ show n) . exploreTreeT $
                    nqueensStart
                        (++)
                        (\solution NQueensBreak90State{..} → liftIO $
                            checkBlocks
                                solution
                                b90_window_start
                                b90_window_size
                                b90_occupied_rows_and_columns
                                b90_occupied_rows_and_columns
                                b90_occupied_negative_diagonals
                                b90_occupied_positive_diagonals
                        )
                        (\solution NQueensBreak180State{..} → liftIO $ do
                            checkBlocks
                                solution
                                b180_window_start
                                b180_window_size
                                b180_occupied_rows
                                b180_occupied_columns
                                b180_occupied_negative_diagonals
                                b180_occupied_positive_diagonals
                            checkRightPositiveBlocks
                                b180_window_size
                                b180_occupied_positive_diagonals
                                b180_occupied_right_positive_diagonals
                        )
                        (\solution window_size NQueensSearchState{..} → liftIO $ do
                            checkBlocks
                                solution
                                s_row
                                window_size
                                s_occupied_rows
                                s_occupied_columns
                                s_occupied_negative_diagonals
                                s_occupied_positive_diagonals
                        )
                        []
                        n
                | n ← [2..20]
                ]
             -- }}}
            ,testGroup "correct symmetries" -- {{{
                [ testCase ("n = " ++ show n) . exploreTreeT $
                    nqueensStart
                        (++)
                        (const . checkSymmetry n AllRotations)
                        (const . checkSymmetry n Rotate180Only)
                        (const . const . checkSymmetry n NoSymmetries)
                        []
                        n
                | n ← [2..20]
                ]
             -- }}}
            ,testGroup "includes all solution exteriors" -- {{{
                [ testCase ("n = " ++ show n) . exploreTreeT $ do
                    let start_exteriors = Set.fromList . map sort $ getAllSolutions n
                    solution ← nqueensBruteForceSolutions n
                    liftIO
                        .
                        assertBool ("solution " ++ show solution ++ " --> " ++ show (sort . map sort . multiplySolution n NoSymmetries $ extractExteriorFromSolution n 1 solution :: [[(Word,Word)]]) ++ " does not have an exterior in the starting set")
                        .
                        any (
                          flip Set.member start_exteriors
                          .
                          sort
                        )
                        .
                        multiplySolution n NoSymmetries
                        $
                        extractExteriorFromSolution n 1 solution
                | n ← [2..11]
                ]
             -- }}}
            ,testGroup "includes all solutions" -- {{{
                [ testCase ("n = " ++ show n) $
                    let finalizeValueWithMultiplicity m original_solution = do
                            liftIO $ checkSolutionIsValid n (sort original_solution)
                            solutions ← lift get
                            solution ← allFrom . remdups . map sort $ do
                                rotated_solution ← take m . iterate (rotateLeft n) $ original_solution
                                allFrom [rotated_solution,reflectSolution n rotated_solution]
                            liftIO $ assertBool ("solution appears twice: " ++ show solution) (not $ Set.member solution solutions)
                            lift $ modify (Set.insert solution)
                    in
                    (flip execStateT Set.empty
                     .
                     exploreTreeT
                     $
                     nqueensStart
                        (++)
                        (\value NQueensBreak90State{..} →
                            nqueensSearch
                                (++)
                                (finalizeValueWithMultiplicity 1)
                                value
                                b90_window_size
                                $
                                NQueensSearchState
                                    b90_number_of_queens_remaining
                                    b90_window_start
                                    b90_occupied_rows_and_columns
                                    b90_occupied_rows_and_columns
                                    b90_occupied_negative_diagonals
                                    b90_occupied_positive_diagonals
                        )
                        (\value NQueensBreak180State{..} → 
                            nqueensSearch
                                (++)
                                (finalizeValueWithMultiplicity 2)
                                value
                                b180_window_size
                                $
                                NQueensSearchState
                                    b180_number_of_queens_remaining
                                    b180_window_start
                                    b180_occupied_rows
                                    b180_occupied_columns
                                    b180_occupied_negative_diagonals
                                    b180_occupied_positive_diagonals
                        )
                        (nqueensSearch (++) (finalizeValueWithMultiplicity 4))
                        []
                        n
                    )
                    >>=
                    (assertEqual "missing solutions" Set.empty
                     .
                     Set.difference (
                         Set.fromList . map sort $ nqueensBruteForceSolutions n
                     )
                    )
                | n ← [4..12]
                ]
             -- }}}
            ,testGroup "unique" -- {{{
                [ testCase ("n = " ++ show n) . flip evalStateT Set.empty . exploreTreeT $ do
                    old_solutions ← lift get
                    solution ← sort <$>
                        getAllSolutions n
                        >>=
                        multiplySolution n NoSymmetries
                    if Set.member solution old_solutions
                        then liftIO $ assertFailure ("solution " ++ show solution ++ " occurs twice")
                        else lift $ modify (Set.insert solution)
                | n ← [1..20]
                ]
             -- }}}
            ,testGroup "valid" $ -- {{{
                map (\n → testCase ("n = " ++ show n) . checkSolutionsAreValid n . getAllSolutions $ n)
                    [1..20]
             -- }}}
            ]
         -- }}}
        ,testGroup "break90" $ -- {{{
            let getAllSolutions :: MonadPlus m ⇒ Word → m [(Word,Word)] -- {{{
                getAllSolutions n = break90 [] $ NQueensBreak90State n 0 (fromIntegral n) 0 0 0
                  where
                    break90 =
                        nqueensBreak90
                            (++)
                            return
                            (\value state → return value `mplus` break90 value state)
                            (const . return)
                            (const . const . return)
            in -- }}}
            [testGroup "correct blocks" -- {{{
                [ testCase ("n = " ++ show n) . exploreTreeT $
                    let break90 =
                            nqueensBreak90
                                (++)
                                (const $ return ())
                                (\solution state@NQueensBreak90State{..} → do
                                    liftIO $
                                      checkBlocks
                                        solution
                                        b90_window_start
                                        b90_window_size
                                        b90_occupied_rows_and_columns
                                        b90_occupied_rows_and_columns
                                        b90_occupied_negative_diagonals
                                        b90_occupied_positive_diagonals
                                    break90 solution state
                                )
                                (\solution NQueensBreak180State{..} → liftIO $ do
                                    checkBlocks
                                        solution
                                        b180_window_start
                                        b180_window_size
                                        b180_occupied_rows
                                        b180_occupied_columns
                                        b180_occupied_negative_diagonals
                                        b180_occupied_positive_diagonals
                                    checkRightPositiveBlocks
                                        b180_window_size
                                        b180_occupied_positive_diagonals
                                        b180_occupied_right_positive_diagonals
                                )
                                (\solution window_size NQueensSearchState{..} → liftIO $
                                    checkBlocks
                                        solution
                                        s_row
                                        window_size
                                        s_occupied_rows
                                        s_occupied_columns
                                        s_occupied_negative_diagonals
                                        s_occupied_positive_diagonals
                                )
                        in break90 [] $ NQueensBreak90State n 0 (fromIntegral n) 0 0 0
                | n ← [2..20]
                ]
             -- }}}
            ,testGroup "correct symmetries" -- {{{
                [ testCase ("n = " ++ show n) . exploreTreeT $
                    let break90 =
                            nqueensBreak90
                                (++)
                                (liftIO . assertEqual "solution has the wrong symmetry" AllRotations . symmetryOf n)
                                (\solution next_state → do
                                    checkSymmetry n AllRotations solution
                                    break90 solution next_state
                                )
                                (const . checkSymmetry n Rotate180Only)
                                (const . const . checkSymmetry n NoSymmetries)
                    in break90 [] $ NQueensBreak90State n 0 (fromIntegral n) 0 0 0
                | n ← [2..20]
                ]
             -- }}}
            ,testGroup "includes all solution exteriors" -- {{{
                [ testCase ("n = " ++ show n) . exploreTreeT $ do
                    let break90_exteriors = Set.fromList . map sort $ getAllSolutions n
                    solution ← nqueensBruteForceSolutions n
                    let maximum_layers = (n+1) `div` 2
                        go layers
                          | layers > maximum_layers = return ()
                          | otherwise =
                                (assertBool ("solution " ++ show solution ++ " --> " ++ show (sort . map sort . allRotationsOf n $ exterior :: [[(Word,Word)]]) ++ " (" ++ show (symmetryOf n exterior) ++ ") does not have a " ++ show layers ++ " exterior in the break90 set")
                                 .
                                 any (
                                   flip Set.member break90_exteriors
                                   .
                                   sort
                                  )
                                  .
                                  allRotationsOf n
                                  $
                                  exterior
                                 ) >> if hasRotate90Symmetry n exterior then go (layers+1) else return ()
                           | otherwise = go (layers+1)
                           where
                             exterior = extractExteriorFromSolution n layers solution
                    liftIO $ go 1
                | n ← [4..12]
                ]
             -- }}}
            ,testGroup "includes all solutions" -- {{{
                [ testCase ("n = " ++ show n) $
                    let finalizeValueWithMultiplicity m original_solution = do
                            liftIO $ checkSolutionIsValid n (sort original_solution)
                            solutions ← lift get
                            solution ← allFrom . remdups . map sort . take m . iterate (rotateLeft n) $ original_solution
                            liftIO $ assertBool ("solution appears twice: " ++ show solution) (not $ Set.member solution solutions)
                            lift $ modify (Set.insert solution)
                        break90 =
                            nqueensBreak90
                                (++)
                                (finalizeValueWithMultiplicity 4)
                                 break90
                                (\value NQueensBreak180State{..} →
                                    nqueensSearch (++) (finalizeValueWithMultiplicity 2) value b180_window_size $
                                        NQueensSearchState
                                            b180_number_of_queens_remaining
                                            b180_window_start
                                            b180_occupied_rows
                                            b180_occupied_columns
                                            b180_occupied_negative_diagonals
                                            b180_occupied_positive_diagonals
                                )
                                (nqueensSearch (++) (finalizeValueWithMultiplicity 4))
                    in (flip execStateT Set.empty
                        .
                        exploreTreeT
                        $
                        break90 [] $ NQueensBreak90State n 0 (fromIntegral n) 0 0 0
                        :: IO (Set NQueensSolution)
                       ) >>= assertEqual "missing solutions" Set.empty
                             .
                             Set.difference (
                                Set.fromList . map sort $ nqueensBruteForceSolutions n
                             )
                | n ← [4..12]
                ]
             -- }}}
            ,testGroup "unique" -- {{{
                [ testCase ("n = " ++ show n) . flip evalStateT Set.empty . exploreTreeT $ do
                    old_solutions ← lift get
                    solution ← sort <$>
                        getAllSolutions n
                        >>=
                        multiplySolution n NoSymmetries
                    if (Set.member solution old_solutions)
                        then liftIO $ assertFailure ("solution " ++ show solution ++ " occurs twice")
                        else lift $ modify (Set.insert solution)
                | n ← [1..20]
                ]
             -- }}}
            ,testGroup "valid" $ -- {{{
                map (\n → testCase ("n = " ++ show n) . checkSolutionsAreValid n . getAllSolutions $ n)
                    [2..20]
             -- }}}
            ]
         -- }}}
        ,testGroup "break180" $ -- {{{
            let getAllSolutions :: MonadPlus m ⇒ Word → m [(Word,Word)] -- {{{
                getAllSolutions n = break180 [] $ NQueensBreak180State n 0 (fromIntegral n) 0 0 0 0 0
                  where
                    break180 =
                        nqueensBreak180
                            (++)
                             return
                            (\value state → return value `mplus` break180 value state)
                            (const . const . return)
            in -- }}}
            [testGroup "correct blocks" -- {{{
                [ testCase ("n = " ++ show n) . exploreTreeT $
                    let break180 =
                            nqueensBreak180
                                (++)
                                (const $ return ())
                                (\solution state@NQueensBreak180State{..} → do
                                    liftIO $ do
                                      checkBlocks
                                        solution
                                        b180_window_start
                                        b180_window_size
                                        b180_occupied_rows
                                        b180_occupied_columns
                                        b180_occupied_negative_diagonals
                                        b180_occupied_positive_diagonals
                                      checkRightPositiveBlocks
                                        b180_window_size
                                        b180_occupied_positive_diagonals
                                        b180_occupied_right_positive_diagonals
                                    break180 solution state
                                )
                                (\solution window_size NQueensSearchState{..} → liftIO $
                                    checkBlocks
                                        solution
                                        s_row
                                        window_size
                                        s_occupied_rows
                                        s_occupied_columns
                                        s_occupied_negative_diagonals
                                        s_occupied_positive_diagonals
                                )
                    in break180 [] $ NQueensBreak180State n 0 (fromIntegral n) 0 0 0 0 0
                | n ← [2..14]
                ]
             -- }}}
            ,testGroup "correct symmetries" -- {{{
                [ testCase ("n = " ++ show n) . exploreTreeT $
                    let break180 =
                            nqueensBreak180
                                (++)
                                (liftIO . assertBool "solution does not have 180 symmetry" . hasRotate180Symmetry n)
                                (\solution next_state → do
                                    liftIO $
                                        assertBool
                                            (printf "solution does not have 180 symmetry: %s" (show solution))
                                            (hasRotate180Symmetry n solution)
                                    break180 solution next_state
                                )
                                (const . const . checkSymmetry n NoSymmetries)
                    in break180 [] $ NQueensBreak180State n 0 (fromIntegral n) 0 0 0 0 0
                | n ← [2..14]
                ]
             -- }}}
            ,testGroup "includes all solution exteriors" -- {{{
                [ testCase ("n = " ++ show n) . exploreTreeT $ do
                    let break180_exteriors = Set.fromList . map sort $ getAllSolutions n
                    solution ← nqueensBruteForceSolutions n
                    let maximum_layers = (n+1) `div` 2
                        go layers
                          | layers > maximum_layers = return ()
                          | otherwise =
                                (assertBool ("solution " ++ show solution ++ " --> " ++ show (sort . map sort . allRotationsOf n $ exterior :: [[(Word,Word)]]) ++ " (" ++ show (symmetryOf n exterior) ++ ") does not have a " ++ show layers ++ " exterior in the break90 set")
                                 .
                                 any (
                                   flip Set.member break180_exteriors
                                   .
                                   sort
                                  )
                                  .
                                  allRotationsOf n
                                  $
                                  exterior
                                 ) >> if hasRotate180Symmetry n exterior then go (layers+1) else return ()
                           | otherwise = go (layers+1)
                           where
                             exterior = extractExteriorFromSolution n layers solution
                    liftIO $ go 1
                | n ← [4..12]
                ]
             -- }}}
            ,testGroup "includes all solutions" -- {{{
                [ testCase ("n = " ++ show n) $
                    let finalizeValueWithMultiplicity multiply original_solution = do
                            liftIO $ checkSolutionIsValid n (sort original_solution)
                            solutions ← lift get
                            let rotated_solutions = remdups . map sort . multiply $ original_solution
                            solution ← allFrom rotated_solutions
                            liftIO $ assertBool (printf "solution appears twice: %s --> %s" (show solution) (show rotated_solutions)) (not $ Set.member solution solutions)
                            lift $ modify (Set.insert solution)
                        break180 =
                            nqueensBreak180
                                (++)
                                (finalizeValueWithMultiplicity $ \x → [x])
                                 break180
                                (nqueensSearch (++) (finalizeValueWithMultiplicity $ \x → [x,rotate180 n x]))
                    in (flip execStateT Set.empty
                        .
                        exploreTreeT
                        $
                        break180 [] $ NQueensBreak180State n 0 (fromIntegral n) 0 0 0 0 0
                        :: IO (Set NQueensSolution)
                       ) >>= assertEqual "missing solutions" Set.empty
                             .
                             Set.difference (
                                Set.fromList . map sort $ nqueensBruteForceSolutions n
                             )
                | n ← [4..12]
                ]
             -- }}}
            ,testGroup "unique" -- {{{
                [ testCase ("n = " ++ show n) . flip evalStateT Set.empty . exploreTreeT $ do
                    old_solutions ← lift get
                    solution ← sort <$>
                        getAllSolutions n
                        >>=
                        multiplySolution n NoSymmetries
                    if (Set.member solution old_solutions)
                        then liftIO $ assertFailure ("solution " ++ show solution ++ " occurs twice")
                        else lift $ modify (Set.insert solution)
                | n ← [1..14]
                ]
             -- }}}
            ,testGroup "valid" $ -- {{{
                map (\n → testCase ("n = " ++ show n) . checkSolutionsAreValid n . getAllSolutions $ n)
                    [2..14]
             -- }}}
            ]
         -- }}}
        ,testGroup "start + break90 + break180" $ -- {{{
            let getAllSolutions :: MonadPlus m ⇒ Word → m [(Word,Word)] -- {{{
                getAllSolutions = nqueensStart (++) callback90 callback180 search []
                  where
                    callback90 value state = return value `mplus` break90 value state
                    callback180 value state = return value `mplus` break180 value state
                    break90 = nqueensBreak90 (++) return callback90 callback180 search
                    break180 = nqueensBreak180 (++) return callback180 search
                    search = const . const . return
            in -- }}}
            [testGroup "correct blocks" -- {{{
                [ testCase ("n = " ++ show n) . exploreTreeT $
                    let callback90 solution state@NQueensBreak90State{..} =do
                            liftIO $
                              checkBlocks
                                solution
                                b90_window_start
                                b90_window_size
                                b90_occupied_rows_and_columns
                                b90_occupied_rows_and_columns
                                b90_occupied_negative_diagonals
                                b90_occupied_positive_diagonals
                            break90 solution state
                        callback180 solution state@NQueensBreak180State{..} = do
                            liftIO $ do
                              checkBlocks
                                solution
                                b180_window_start
                                b180_window_size
                                b180_occupied_rows
                                b180_occupied_columns
                                b180_occupied_negative_diagonals
                                b180_occupied_positive_diagonals
                              checkRightPositiveBlocks
                                b180_window_size
                                b180_occupied_positive_diagonals
                                b180_occupied_right_positive_diagonals
                            break180 solution state
                        callbackSearch solution window_size NQueensSearchState{..} = liftIO $
                            checkBlocks
                                solution
                                s_row
                                window_size
                                s_occupied_rows
                                s_occupied_columns
                                s_occupied_negative_diagonals
                                s_occupied_positive_diagonals
                        break90 =
                            nqueensBreak90
                                (++)
                                (const $ return ())
                                callback90
                                callback180
                                callbackSearch
                        break180 =
                            nqueensBreak180
                                (++)
                                (const $ return ())
                                callback180
                                callbackSearch
                    in nqueensStart
                        (++)
                        callback90
                        callback180
                        callbackSearch
                        []
                        n
                | n ← [2..14]
                ]
             -- }}}
            ,testGroup "correct symmetries" -- {{{
                [ testCase ("n = " ++ show n) . exploreTreeT $
                    let callback90 solution state = do
                            checkSymmetry n AllRotations solution
                            break90 solution state
                        callback180 solution next_state = do
                            liftIO $
                                assertBool
                                    (printf "solution does not have 180 symmetry: %s" (show solution))
                                    (hasRotate180Symmetry n solution)
                            break180 solution next_state
                        callbackSearch =
                            const . const . checkSymmetry n NoSymmetries
                        break90 =
                            nqueensBreak90
                                (++)
                                (liftIO . assertEqual "solution has the correct symmetry" AllRotations . symmetryOf n)
                                callback90
                                callback180
                                callbackSearch
                        break180 =
                            nqueensBreak180
                                (++)
                                (liftIO . assertEqual "solution has the correct symmetry" Rotate180Only . symmetryOf n)
                                callback180
                                callbackSearch
                    in nqueensStart
                        (++)
                        callback90
                        callback180
                        callbackSearch
                        []
                        n
                | n ← [2..14]
                ]
             -- }}}
            ,testGroup "includes all solution exteriors" -- {{{
                [ testCase ("n = " ++ show n) . exploreTreeT $ do
                    let all_exteriors = Set.fromList . map sort $ getAllSolutions n
                    solution ← nqueensBruteForceSolutions n
                    let maximum_layers = (n+1) `div` 2
                        go layers
                          | layers > maximum_layers = return ()
                          | otherwise =
                                (assertBool ("solution " ++ show solution ++ " --> " ++ show (sort . map sort . allRotationsAndReflectionsOf n $ exterior :: [[(Word,Word)]]) ++ " (" ++ show (symmetryOf n exterior) ++ ") does not have a " ++ show layers ++ " exterior in the set")
                                 .
                                 any (
                                   flip Set.member all_exteriors
                                   .
                                   sort
                                  )
                                  .
                                  allRotationsAndReflectionsOf n
                                  $
                                  exterior
                                 ) >> if symmetryOf n exterior > NoSymmetries then go (layers+1) else return ()
                           | otherwise = go (layers+1)
                           where
                             exterior = extractExteriorFromSolution n layers solution
                    liftIO $ go 1
                | n ← [2..12]
                ]
             -- }}}
            ,testGroup "includes all solutions" -- {{{
                [ testCase ("n = " ++ show n) $
                    let finalizeValueWithSymmetry symmetry original_solution = do
                            liftIO $ checkSolutionIsValid n (sort original_solution)
                            liftIO $ checkSymmetry n symmetry (sort original_solution)
                            solutions ← lift get
                            let multiplied_solutions = map sort . multiplySolution n symmetry $ original_solution
                            solution ← allFrom multiplied_solutions
                            liftIO $ assertBool (printf "solution appears twice: %s --> %s" (show solution) (show multiplied_solutions)) (not $ Set.member solution solutions)
                            lift $ modify (Set.insert solution)
                        break90 =
                            nqueensBreak90
                                (++)
                                (finalizeValueWithSymmetry AllRotations)
                                 break90
                                 break180
                                 search
                        break180 =
                            nqueensBreak180
                                (++)
                                (finalizeValueWithSymmetry Rotate180Only)
                                 break180
                                 search
                        search =
                            nqueensSearch
                                (++)
                                (finalizeValueWithSymmetry NoSymmetries)
                    in (flip execStateT Set.empty
                        .
                        exploreTreeT
                        $
                        nqueensStart
                            (++)
                             break90
                             break180
                             search
                             []
                             n
                        :: IO (Set NQueensSolution)
                       ) >>= assertEqual "missing solutions" Set.empty
                             .
                             Set.difference (
                                Set.fromList . map sort $ nqueensBruteForceSolutions n
                             )
                | n ← [4..12]
                ]
             -- }}}
            ,testGroup "unique" -- {{{
                [ testCase ("n = " ++ show n) . flip evalStateT Set.empty . exploreTreeT $ do
                    old_solutions ← lift get
                    solution ← sort <$>
                        getAllSolutions n
                        >>=
                        multiplySolution n NoSymmetries
                    if (Set.member solution old_solutions)
                        then liftIO $ assertFailure ("solution " ++ show solution ++ " occurs twice")
                        else lift $ modify (Set.insert solution)
                | n ← [1..14]
                ]
             -- }}}
            ,testGroup "valid" $ -- {{{
                map (\n → testCase ("n = " ++ show n) . checkSolutionsAreValid n . getAllSolutions $ n)
                    [2..14]
             -- }}}
            ]
         -- }}}
        ]
     -- }}}
    ,testGroup "brute force solutions" -- {{{
        [testGroup "are valid" $ -- {{{
            map (\n → testCase ("n = " ++ show n) $ checkSolutionsAreValid n (nqueensBruteForceSolutions n))
                [1..10]
         -- }}}
        ,testGroup "are unique" $ -- {{{
            [ testCase ("n = " ++ show n) $
                let solutions_as_list = nqueensBruteForceSolutions n
                    solutions_as_set = Set.fromList solutions_as_list
                in length solutions_as_list @?= Set.size solutions_as_set
            | n ← [1..10]
            ]
         -- }}}
        ,testGroup "match count" -- {{{
            [ testCase ("n = " ++ show n) $
                (correct_count @=?)
                .
                getWordSum
                .
                exploreTree
                .
                nqueensBruteForceCount
                $
                n
            | n ← [1..10]
            , let correct_count = nqueensCorrectCount n
            ]
         -- }}}
        ]
     -- }}}
    ,testGroup "C solutions" -- {{{
        [testGroup "are valid" $ -- {{{
            map (\n → testCase ("n = " ++ show n) $ checkSolutionsAreValid n (nqueensCSolutions n))
                [1..10]
         -- }}}
        ,testGroup "are unique" $ -- {{{
            [ testCase ("n = " ++ show n) $
                let solutions_as_list = nqueensCSolutions n
                    solutions_as_set = Set.fromList solutions_as_list
                in length solutions_as_list @?= Set.size solutions_as_set
            | n ← [1..10]
            ]
         -- }}}
        ,testGroup "match count" -- {{{
            [ testCase ("n = " ++ show n) $
                (correct_count @=?)
                .
                getWordSum
                .
                exploreTree
                .
                nqueensCCount
                $
                n
            | n ← [1..10]
            , let correct_count = nqueensCorrectCount n
            ]
         -- }}}
        ]
     -- }}}
    ,testGroup "solutions" $ testSolutionsUsing nqueensSolutions nqueensCount
    ,testGroup "solutions using sets" $ testSolutionsUsing nqueensUsingSetsSolutions nqueensUsingSetsCount
    ,testGroup "solutions using bits" $ testSolutionsUsing nqueensUsingBitsSolutions nqueensUsingBitsCount
    ]
-- }}}
