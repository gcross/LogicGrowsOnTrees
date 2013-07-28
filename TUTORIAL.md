This file contains a tutorial for using this package. The first part explains
through examples how to do logic programming in Haskell using MonadPlus. The
second part explains how to take a logic program in the form of a Tree (which is
an instance of MonadPlus) and use the infrastructure in this package to run it
in parallel.


Logic programming
=================

In this part we shall show how to write logic programs through the use of three
example problems:  generating ordered pairs of integers, finding valid map
colorings, and finding ways to place n queens on an n x n chess board.


Ordered Pairs of Integers
-------------------------

Logic programming in Haskell is about making choices and applying constraints.
A simple example is the following:


```haskell
pairs :: MonadPlus m => Int -> Int -> m (Int,Int)
pairs max_x max_y = do
    x <- between 1 max_x
    y <- between 1 max_y
    guard $ x < y
    return (x,y)
```

This program generates all pairs of integers within the given ranges such that
the first element of the pair is less than the second element of the pair. The
first line in the pairs function body,

```haskell
x <- between 1 max_x
```

makes a non-deterministic choice for x that is between 1 and max_x (inclusive),
and likewise for y; the between function is part of the LogicGrowsOnTrees
module.  The third line in the function body is a guard that succeeds if x < y
and fails otherwise;  failure results in backtracking to try another choice of
x and/or y.

pairs can return a value that is an instance of an arbitrary type --- that is,
one the caller can choose --- that is an instance of MonadPlus.  For example,
if let you let m be Maybe then pairs will return either nothing if no choices
of x and y satisfy the guard and otherwise it will return a Just value with the
first found solution.  If you let m be the list type then the function will
return the list of all solutions.

This function illustrates the basic functionality but it is not a good example
of how you would actually generate such pairs because a better implementation is
given by:

```haskell
pairs max_x max_y = do
    x <- between 1 max_x
    y <- between (x+1) max_y
    return (x,y)
```

This is more efficient because it restricts the choice of y to only those values
that will satisfy the constraint. This is actually an important optimization
that one should always try to make when working on non-trivial problems: when
possible, one should enforce a constraint by reducing the set of available
choices to those that meet the constraint rather than by generating a larger set
of choices and then applying a filter to eliminate those that don't meet the
constraint.


Map Coloring
------------

For our next example, we consider the problem of coloring a map.  That is, we
are given a list of countries, a relation that tells us which are adjacent, and
a list of colors, and our goal is to find a way to choose a color for each
country such that no two adjacent countries have the same color.  To keep things
simple, we will assume that the colors are numbered from 1 to number_of_colors
and the countries are numbered from 1 to number_of_countries.  A function that
generates solutions to this problem is as follows:

```haskell
coloringSolutions :: MonadPlus m => Word -> Word -> (Word -> Word -> Bool) -> m [(Word,Word)]
coloringSolutions number_of_colors number_of_countries isAdjacentTo =
    go number_of_countries []
  where
    go 0 coloring = return coloring
    go country coloring = do
        color <- between 1 number_of_colors
        forM_ coloring $ \(other_country, other_color) ->
            when (country `isAdjacentTo` other_country) $
                guard (color /= other_color)
        go (country-1) ((country,color):coloring)
```

The meat of this function is in the nested function go.  For each country,
starting from the last country and ending at country 1, the function go does the
following:

1. First, it makes a non-deterministic choice for the color of the country:

```haskell
color <- between 1 number_of_colors
```

2. Second, it checks that all other countries that have been colored and which
   are adjacent to the current country are a different color:

```haskell
forM_ coloring $ \(other_country, other_color) ->
    when (country `isAdjacentTo` other_country) $
        guard (color /= other_color)
```

   The first line of this snippet is a standard construct for looping over a
   list and executing an action for each element.  The second line checks to see
   whether the current country in the loop is adjecent to the country we just
   colored, and if so then the third line checks that the two adjacent countries
   have different colors and fails if this is not the case.

3. Finally, it adds this country's color to the coloring and recursively calls
   itself with the next country:

```haskell
go (country-1) ((country,color):coloring)
```

When all countries have been colored --- that is, when the current country is 0
--- then go returns.

A major inefficiency in the code above is that a large number of the solutions
generated are equivalent in the sense that they only differ by a permutation of
the colors and the selection of the colors used (when this is less than the
total number of colors). In particular, if all n colors are used in a given
solution then there are n! equivalent solutions, if n-1 of the n colors are used
then there are n!/2 equivalent solutions, etc; in general if m colors are used
out of n for a given solution then there are n!/(n-m)! equivalent solutions.

The solution to this is a trick I like to call "symmetry breaking", where you
a symmetry (in this case, the fact that permuting the colors and/or changing the
choice of colors results in an equivalent solution) and factor it out by forcing
a particular ordering.  The following code does this:

```haskell
coloringUniqueSolutions number_of_colors number_of_countries isAdjacentTo =
    go 0 number_of_countries []
  where
    go _ 0 coloring = return coloring
    go number_of_colors_used country coloring = do
        color <- between 1 ((number_of_colors_used + 1) `min` number_of_colors)
        forM_ coloring $ \(other_country, other_color) ->
            when (country `isAdjacentTo` other_country) $
                guard (color /= other_color)
        go (number_of_colors_used `max` color) (country-1) ((country,color):coloring)
```

The above is a modified version of coloringSolutions where we *force* the first
color chosen to be 1, the second color chosen (if not the same as the first) to
be 2, and so on. Specifically, we keep track of the number of colors used so
far, and when the next color is chosen we restrict ourselves to these colors
plus the next greatest color if we have not already used all the available
colors; if our choice involves a new color then we bump up the number of colors
used for the next recursive call to go, otherwise we use the same set of colors.


N-Queens Problem
----------------

Our final example is the n-queens problem, which is the problem of placing n
queens on an n x n board such that none of the queens conflict;  recall that in
chess two queens conflict if they share the same row, column, or diagonal.  A
function that generates solutions to this problem is as follows:

```haskell
import qualified Data.IntSet as IntSet
...
nqueensUsingSetsSolutions :: MonadPlus m => Word -> m [(Word,Word)]
nqueensUsingSetsSolutions n =
    go (fromIntegral n)
       0
       (IntSet.fromDistinctAscList [0..fromIntegral n-1])
       IntSet.empty
       IntSet.empty
       []
  where
    go 0 _ _ _ _ !value = return . reverse $ value
    go !n
       !row
       !available_columns
       !occupied_negative_diagonals
       !occupied_positive_diagonals
       !value
     = do
        column <- allFrom $ IntSet.toList available_columns
        let negative_diagonal = row + column
        guard $ IntSet.notMember negative_diagonal occupied_negative_diagonals
        let positive_diagonal = row - column
        guard $ IntSet.notMember positive_diagonal occupied_positive_diagonals
        go (n-1)
           (row+1)
           (IntSet.delete column available_columns)
           (IntSet.insert negative_diagonal occupied_negative_diagonals)
           (IntSet.insert positive_diagonal occupied_positive_diagonals)
           ((fromIntegral row,fromIntegral column):value)
```

(The use of fromIntegral comes from the fact that the input board size and
output board positions are naturally Words as they cannot be negative but the
IntSet type only stores Ints, which means that we need to work internally using
Int and use fromIntegral to convert from the input Word and to the output
Words.)

The function go is where most of the work happens.  For each row in the board,
it does the following:

1. First, make a non-deterministic choice from the available columns;  here we
   use the function allFrom (part of the LogicGrowsOnTrees module) to convert
   the list of available columns to a MonadPlus that generates it.

```haskell
column <- allFrom $ IntSet.toList available_columns
```

2. Next, we check if this choice of column conflicts with the positive and
   negative diagonals, and if so we backtrack and try a different column:

```haskell
let negative_diagonal = row + column
guard $ IntSet.notMember negative_diagonal occupied_negative_diagonals
let positive_diagonal = row - column
guard $ IntSet.notMember positive_diagonal occupied_positive_diagonals
```

3. Finally, we recursively call go for the next row with updated values for the
   updated available columns and occupied diagonals, as well as the chosen board
   position added to the (partial) solution:

```haskell
go (n-1)
   (row+1)
   (IntSet.delete column available_columns)
   (IntSet.insert negative_diagonal occupied_negative_diagonals)
   (IntSet.insert positive_diagonal occupied_positive_diagonals)
   ((fromIntegral row,fromIntegral column):value)
```

When we are done, we reverse the solution as it currently has the last row
first and the first row last.

While IntSet is very efficient, even more efficient is to use a bit field for a
set --- i.e., such that bits that are 1 correspond to being occupied and those
that are 0 correspond to being available.  A solution using this approach is as
follows:

```haskell
nqueensUsingBitsSolutions n =
    go n 0 (0::Word64) (0::Word64) (0::Word64) []
  where
    go 0 _ _ _ _ !value = return . reverse $ value
    go !n
       !row
       !occupied_columns
       !occupied_negative_diagonals
       !occupied_positive_diagonals
       !value
     = do
        column <- allFrom . goGetOpenings 0 $
            occupied_columns .|. 
            occupied_negative_diagonals .|.
            occupied_positive_diagonals
        let column_bit = bit (fromIntegral column)
        go (n-1)
           (row+1)
           (occupied_columns .|. column_bit)
           ((occupied_negative_diagonals .|. column_bit) `shiftR` 1)
           ((occupied_positive_diagonals .|. column_bit) `shiftL` 1)
           ((row,column):value)

    goGetOpenings column bits
      | column >= n     = []
      | bits .&. 1 == 0 = column:next
      | otherwise       = next
      where
        next = goGetOpenings (column + 1) (bits `shiftR` 1)
```

Now in the function go we do the following:

1. Make a non-deterministic choice of the column, excluding those columns which
   are either occupied or such that a queen placed there would conflict with
   another queen on the same diagonal:

```haskell
column <- allFrom . goGetOpenings 0 $
    occupied_columns .|. 
    occupied_negative_diagonals .|.
    occupied_positive_diagonals
let column_bit = bit (fromIntegral column)
```

2. Mark the column and diagonals as being occupied, and also shift the occupied
   diagonals so that they correspond with the columns in the next row.  That is,
   if a given positive diagonal intersects with column i for a given row then it
   intersects with column i+1 in the next row, and if a given negative diagonal
   intersects with column i for a given row then it intersects with i-1 in the
   next row.  Finally, add the board position to the partial solution:

```haskell
go (n-1)
   (row+1)
   (occupied_columns .|. column_bit)
   ((occupied_negative_diagonals .|. column_bit) `shiftR` 1)
   ((occupied_positive_diagonals .|. column_bit) `shiftL` 1)
   ((row,column):value)
```

The new nested function getOpenings scans through the input bits and builds a
list of columns where a queen may be placed without conflict.  If there are no
such columns, then the list will be empty and the code will backtrack.

It is possible to use symmetry breaking gain a significant speed-up, but the
solution I came up with that does this ended up being quite complicated.  You
can see it for yourself in the LogicGrowsOnTrees.Examples.Queens.Advanced
module.