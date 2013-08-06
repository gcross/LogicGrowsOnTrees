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


Using LogicGrowsOnTrees
=======================

In the preceding part, we gave some examples of how to write a logic program
using MonadPlus. In this part we will talk about how to use this package to with
such programs, and in particular how to run them in parallel. First we will show
how to use this package to run a logic program serially, which is done for two
reasons: first, to introduce the functionality of this package in a simpler
setting, and second, because it is useful when one is testing a progrem. Next we
will show how to run a logic program in parallel using the Threads module.
Finally, we will show how to run a logic program in parallel using the Main
module, which provides a universal interface to all adapters as well as
automating the work required to specify a command line interface.

Note that the examples in the preceding part have all been implemented in the
LogicGrowsOnTrees.Examples.* modules, so in the examples to follow we will
reference them by importing them for their corresponding module rather than by
copying and pasting their source code.

Serial
------

The central data structure in LogicGrowsOnTrees is the Tree type, which, as the
name suggests, is a tree data structure where each branch corresponds to a
binary choice made in a logic program by the `mplus` function, which takes two
trees and returns a new tree whose root is a choice between the two arguments.
We have not been using this function directly because we have wanted to make
choices from a list of elements, and so we used the `allFrom` function to
essentially convert this list to a tree using `mplus`.

Because Tree is an instance of MonadPlus, all of the logic programs we have
written are automatically available in Tree form. Thus, to run a logic program
in serial, you can use the exploreTree* family of functions in
LogicGrowsOnTreee.

The exploreTree function explores the entire tree and sums over all the results;
for this reason, the result type of the tree needs to be a Monoid.  This means
that in particular if you, say, want to get a list of all solutions, then your
logic program needs to put each result in a list singleton so that the sum
builds up the list of all solutions.  The following program is an example of
applying this to prints a list of all the n-queens solutions for n = 5 (see
tutorial/tutorial-1.hs):

```haskell
import LogicGrowsOnTrees (exploreTree)
import LogicGrowsOnTrees.Examples.Queens (nqueensUsingBitsSolutions)

main = print . exploreTree . fmap (:[]) . nqueensUsingBitsSolutions $ 5
```

Note the use of `fmap (:[])` to effetively replace every result generated by the
logic program by a singleton list containing the result.  When there are a lot
of results it is better to use the Seq type in Data.Sequence as it has
asymptotically faster concatenation operations than List, such as in the
following (see tutorial/tutorial-2.hs):

```haskell
import qualified Data.Sequence as Seq
import LogicGrowsOnTrees (exploreTree)
import LogicGrowsOnTrees.Examples.Queens (nqueensUsingBitsSolutions)

main = print . exploreTree . fmap Seq.singleton . nqueensUsingBitsSolutions $ 5
```

Alternatively, if you are only intersted in the *number* of solutions rather
than what they are, then you should replace every solution with `WordSum 1`,
where WordSum is a Monoid (included part of this package in the module
LogicGrowsTrees.Utils.WordSum) with the property that the sum of two WordSums is
a WordSum containing the sum of the two contained values; this is done in the
following (see tutorial/tutorial-3.hs):

```haskell
import LogicGrowsOnTrees (exploreTree)
import LogicGrowsOnTrees.Utils.WordSum (WordSum(..))
import LogicGrowsOnTrees.Examples.Queens (nqueensUsingBitsSolutions)

main = print . exploreTree . fmap (const $ WordSum 1) . nqueensUsingBitsSolutions $ 5
```

Note that the only change is that we replaced `fmap Seq.singleton` with
`fmap (const $ WordSum 1)`.

If you only want the first result then you should use exploreTreeUntilFirst, which return
the first result found wrapped in Just if any results are present and Nothing if
no results were found;  for example, see the following
(see tutorial/tutorial-4.hs):

```haskell
import LogicGrowsOnTrees (exploreTreeUntilFirst)
import LogicGrowsOnTrees.Examples.Queens (nqueensUsingBitsSolutions)

main = print . exploreTreeUntilFirst . nqueensUsingBitsSolutions $ 10
```

Finally, if you only want a few of the results, then use exploreTreeUntilFound,
which takes a condition function and will stop finding new results when it is
met; the result is a pair where the first component contains the results that
were found and the second contains a Bool indicating whether the conditions was
met. Note that the returned results might be less than those requested if there
weren't enough found to meet your condition function, and it also might be
*more* than those requested because results are not found one at a time but
rather are merged from the bottom up, meaning that there might be a choice point
where the two branches separately did not meet the condition but their merged
results did. An example of using this to find at least 3 results is illustrated
as follows (see tutorial/tutorial-5.hs):

```haskell
import LogicGrowsOnTrees (exploreTreeUntilFound)
import LogicGrowsOnTrees.Examples.Queens (nqueensUsingBitsSolutions)

main =
    print
    .
    exploreTreeUntilFound ((>= 3) . length)
    .
    fmap (:[])
    .
    nqueensUsingBitsSolutions
    $
    10
```

The above will print a pair where the first component has *five* solutions and
the second component is true.  Note the condition function `((>= 3) . length)`
which computes the length of the list and checks whether it is at least 3.

NOTE: If for some reason you really don't want more than, say, 3 solutions ---
say, because the solutions are very large and you never want to keep around more
than 3 --- then your best bet is to create a custom Monoid type that, say,
contains a list and never allows concatenation to let it grow bigger than 3
elements.


Parallel using Threads
----------------------

The LogicGrowsOnTrees.Parallel.Adapter.Threads module provides functions that
let you run a logic program in parallel using multiple threads.  You have a
couple of different options for doing this.

First, you can use one of the many specialized functions which roughly follow
the same pattern as the exploreTree* functions in LogicGrowsOnTrees, such as the
following (see tutorial/tutorial-6.hs):

```haskell
import Control.Monad (void)

import LogicGrowsOnTrees.Parallel.Adapter.Threads
    (RunOutcome(..)
    ,TerminationReason(..)
    ,changeNumberOfWorkers
    ,exploreTree
    )
import LogicGrowsOnTrees.Utils.WordSum (WordSum(..))
import LogicGrowsOnTrees.Examples.Queens (nqueensUsingBitsSolutions)

main = do
    RunOutcome statistics termination_reason <-
        exploreTree (void . changeNumberOfWorkers . const . return $ 2)
        .
        fmap (const $ WordSum 1)
        .
        nqueensUsingBitsSolutions
        $
        10
    case termination_reason of
        Aborted progress -> putStrLn "Count aborted."
        Completed (WordSum count) -> putStrLn $ "Found " ++ show count ++ " solutions."
        Failure progress message -> putStrLn $ "Failed: " ++ message
```

(Note:  Although the above uses two threads, the two threads will not actually
run in parallel unless you compile with the options "-threaded" to use the
threaded run-time and then also run the program with "+RTS -N2" to tell the
runtime (+RTS) that you want it to spawn two OS threads (-N2).)

First, observe that exploreTree now has an additional argument:

```haskell
void . changeNumberOfWorkers . const . return $ 2
```

This argument is called the *controller*, and it is a loop that is run that lets
you issue commands to the supervisor such as abort, requesting a progress
update, and changing the number of workers (which can be doen at any time in the
run and can even bring the number down to zero).  changeNumberOfWorkers takes a
single argument which is a function that maps the current number of workers to
an IO action whose result is the new number of workers;  this lets you do things
like increasing the number of workers by one and setting the number of workers
to a value that you need to query in the IO monad such as the number of
capabilities;  the `void` at the beginning just throws out the return value of
changeNumberOfWorkers, which is the new number of workers (as if you just
increased it by 1 then you might want to know what the result was).

Next, in the first line of the body of the main function, we have

```haskell
RunOutcome statistics termination_reason <-
```

Now that we are running many workers in parallel, the result of the exploration
is a bit more complicated.  The result type is a RunOutcome, which contains the
run statistics and the termination reason.  The run statistics contain a lot of
information whose primary purpose is to help one diagnose why one is not getting
the appropriate speedup as one increases the number of workers.  The termination
reason contains information about why the run terminated.  As you can see at the
end of the code, there are three possibilities:

```haskell
case termination_reason of
    Aborted progress -> putStrLn "Count aborted."
    Completed (WordSum count) -> putStrLn $ "Found " ++ show count ++ " solutions."
    Failure message -> putStrLn $ "Failed: " ++ message
```

First, we have `Aborted`, which means that a request was made to abort the run;
it contains the progress that had been made up to that point, which can be used
to resume the run at the same point later.

Second, we have `Completed`, which means that the run terminated normally; it
contains the final result in the run, which in this case is a Word wrapped in
a WordSum.

Finally, we have `Failure`, which indicates that something horribly wrong
happened during the run, such as an exception being thrown;  it contains a
message that describes what happened.  If your logic program is pure, then this
most likely means that there is a bug somewhere in your program.  (If it is not
pure, which we have not covered in this tutorial, a `Failure` might just mean
that, say, an external resource that is needed by the program was not
available.)

In the case of both `Aborted` and `Failure`, there is an argument which
represents the current progress of the exploration, which you can use as the
starting point by calling the `exploreTreeStartingFrom` function.

There is an important caveat, however: it only makes sense to resume an
exploration using a checkpoint *if you have not changed the program*, because in
general if you change the program then you change the Tree, which means that the
checkpoint is no longer a valid; in particular, if the explored part of the tree
changes then your current sum over results is no longer correct, and if the
shape of the tree changes then in general the checkpoint will not line up with
it and will raise an error. (Having said this, if you make a change that only
affects parts of the tree that have not been explored then you *might* be fine.)

Because of this, it will rarely make sense to resume from a Failure if your
program is pure because an exception will almost always signal the presence of
a big.  The main reason for including the progress with the Failure is because,
although we have not discussed this, it is possible to write logic programs that
run in the I/O monad and requires access to, say, a database server;  if the
database server goes down then it makes perfect sense to restart it and then
resume the run from the last progress.

In the following code, we show an example of resuming after aborting as well as
of a non-trivial controller (see tutorial/tutorial-7.hs):

```haskell
import Control.Monad.IO.Class (liftIO)
import Data.Monoid (mempty)
import System.Exit (exitFailure,exitSuccess)

import LogicGrowsOnTrees.Checkpoint (Progress(..))
import LogicGrowsOnTrees.Parallel.Adapter.Threads
    (RunOutcome(..)
    ,TerminationReason(..)
    ,abort
    ,changeNumberOfWorkers
    ,exploreTreeStartingFrom
    ,requestProgressUpdate
    )
import LogicGrowsOnTrees.Utils.WordSum (WordSum(..))
import LogicGrowsOnTrees.Examples.Queens (nqueensUsingBitsSolutions)

main = go mempty
 where
  go progress@(Progress _ (WordSum count)) = do
    putStrLn $ "Counting... (starting with " ++ show count ++ " solutions); press <Enter> to abort"
    RunOutcome _ termination_reason <-
        exploreTreeStartingFrom
            progress
            (do _ <- changeNumberOfWorkers . const . return $ 2
                _ <- liftIO $ getLine
                _ <- requestProgressUpdate
                abort
            )
        .
        fmap (const $ WordSum 1)
        .
        nqueensUsingBitsSolutions
        $
        14
    case termination_reason of
        Aborted progress -> do
            putStrLn $ "Count aborted; will try again."
            go progress
        Completed (WordSum count) -> do
            putStrLn $ "Found " ++ show count ++ " solutions."
            exitSuccess
        Failure _ message -> do
            putStrLn $ "Failed: " ++ message
            exitFailure
```

(Note:  If this code takes too much or too little time to finish on your
computer then you can adjust the problem size, which is currently set to 14.)

This code features a number of differences from the previous example.  First we
note that the controller is non-trivial:

```haskell
(do _ <- changeNumberOfWorkers . const . return $ 2
    _ <- liftIO $ getLine
    _ <- requestProgressUpdate
    abort
)
```

Whereas previously the controller just set the number of workers and then quit,
now it insteads waits for the user to press enter;  if the user does so, then
the controller tells the supervisor that it should perform a progress update ---
that is, that it should contact all the workers and fetch their current results
and checkpoints --- and then finally it tells the supervisor to abort.

Next, note that main function is a loop:

```haskell
main = go mempty
 where
  go progress@(Progress _ (WordSum count)) = do
    ...
    case termination_reason of
        Aborted progress -> do
            putStrLn $ "Count aborted; will try again."
            go progress
        Completed (WordSum count) -> do
            putStrLn $ "Found " ++ show count ++ " solutions."
            exitSuccess
        Failure _ message -> do
            putStrLn $ "Failed: " ++ message
            exitFailure
```

Specifically, as long as the user keeps aborting by pressing enter, the code
will loop and immediately resume starting from its progress at the time the run
was aborted.  If the run finishes by either terminating successfully or with a
failure, then the program exits.  You don't have to worry about making sure that
the controller terminates because if the run terminates for any reason then it
kills the controller thread rather than leaving it hanging.

For an example of a more useful controller, see the following
(tutorial/tutorial-8.hs):

```haskell
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
```

Now the controller continually polls the user for the desired number of workers
and then changes the number of workers to be equal to it.

As in the serial case, there are multiple modes in which an exploration can be
run.  The following code only looks for the first result (see tutorials/tutorial-9.hs):

```haskell
import Control.Monad (void)

import LogicGrowsOnTrees.Parallel.Adapter.Threads
    (RunOutcome(..)
    ,TerminationReason(..)
    ,changeNumberOfWorkers
    ,exploreTreeUntilFirst
    )
import LogicGrowsOnTrees.Checkpoint (Progress(..))
import LogicGrowsOnTrees.Examples.Queens (nqueensUsingBitsSolutions)

main = do
    RunOutcome statistics termination_reason <-
        exploreTreeUntilFirst (void . changeNumberOfWorkers . const . return $ 2)
        .
        nqueensUsingBitsSolutions
        $
        10
    case termination_reason of
        Aborted _ -> putStrLn "Search aborted."
        Completed Nothing -> putStrLn "No result found."
        Completed (Just (Progress checkpoint result)) -> putStrLn $ "Found " ++ show result
        Failure _ message -> putStrLn $ "Failed: " ++ message
```

Note how now `Completed` contains a `Maybe` value:

```haskell
Completed Nothing -> putStrLn "No solution found."
Completed (Just (Progress checkpoint result)) -> putStrLn $ "Found " ++ show result
```

If the run finds no solution, then it returns Nothing.  If it does return a
solution, then it returns a Progress value, which contains not only the
solution but also the checkpoint;  the reason for returning the checkpoint is
that it allows you to resume from it if you decide that you want to find more
solutions at some point in the future.

As in the serial case, you can also request that only some of the results be
found, as in the following code which looks for at least five solutions (see
tutorials/tutorial-10.hs):

```haskell
import Control.Monad (void)

import LogicGrowsOnTrees.Parallel.Adapter.Threads
    (RunOutcome(..)
    ,TerminationReason(..)
    ,changeNumberOfWorkers
    ,exploreTreeUntilFoundUsingPush
    )
import LogicGrowsOnTrees.Checkpoint (Progress(..))
import LogicGrowsOnTrees.Examples.Queens (nqueensUsingBitsSolutions)

main = do
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
```

Now the result in `Completed` is `Left results` if the run ended before the
condition function was satisfed and `Right (Progress checkpoint results)`
otherwise, where again the `checkpoint` value allows you to resume the search at
some point in the future if you wish.

There is also an `exploreTreeUntilFoundUsingPull` function which is similar to
this function except that it gathers the results in a different way. The
difference between them is that `exploreTreeUntilFoundUsingPull` sums results
locally on each worker until either a progress update is requested or the
condition is satisfied, whereas `exploreTreeUntilFoundUsingPush` pushes each
result to the supervisor immediately as soon as it is found. If you are only
looking for a few results then `exploreTreeUntilFoundUsingPush` is preferable
because the whole system will know right away when the desired results have been
found. If you are looking for a large number of results then the overhead of
sending each result to the supervisor may add up to the point where it is better
to sum locally and only send results to the supervisor periodically; note that
if you take the latter approach then it is your responsibility to have the
controller periodically request progress updates. (Note that the
`LogicGrowsOnTrees.Parallel.Common.RequestQueue` module has a `fork` function
that you can use to spawn another controller thread if this would make your life
easier;  like the main controller thread it will be killed when the run is
over.)
