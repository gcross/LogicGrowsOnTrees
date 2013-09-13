This file contains a tutorial for using this package. The first part explains
through examples how to do logic programming in Haskell using `MonadPlus`. The
second part explains how to take a logic program in the form of a `Tree` (which
is an instance of `MonadPlus`) and use the infrastructure in this package to run
it in parallel.


Logic programming
=================

In this part we shall show how to write logic programs through the use of three
example problems:  generating ordered pairs of integers, finding valid map
colorings, and finding ways to place n queens on an n x n chess board.


Ordered pairs of integers
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
first line in the body of `pairs`,

```haskell
x <- between 1 max_x
```

makes a non-deterministic choice for `x` that is between `1` and `max_x`
(inclusive), and likewise for `y`; the `between` function is part of the
`LogicGrowsOnTrees` module. The third line in the function body is a guard that
succeeds if `x < y` and fails otherwise; failure results in backtracking to try
another choice of `x` and/or `y`.

`pairs` returns a value that can be an instance of an arbitrary type --- i.e.,
one the caller can choose --- so long as it is an instance of `MonadPlus`. For
example, if let you let `m` be `Maybe` then `pairs` will return either nothing
if no choices of `x` and `y` satisfy the guard and otherwise it will return a
`Just` value with the first found solution. If you let `m` be the List type then
the function will return the list of all solutions.

This function illustrates the basic functionality but it is not a good example
of how you would actually generate such pairs; a better implementation is given
by:

```haskell
pairs max_x max_y = do
    x <- between 1 max_x
    y <- between (x+1) max_y
    return (x,y)
```

This is more efficient because it restricts the choice of `y` to only those
values that will satisfy the constraint. This is actually an important
optimization that one should always try to make when working on non-trivial
problems: when possible, one should enforce a constraint by reducing the set of
available choices to those that meet the constraint rather than by generating a
larger set of choices and then applying a filter to eliminate those that don't
meet the constraint.


Map coloring
------------

For our next example, we consider the problem of coloring a map. That is, we are
given a list of countries, a relation that tells us which are adjacent, and a
list of colors, and our goal is to find a way to choose a color for each country
such that no two adjacent countries have the same color. To keep things simple,
we will assume that the colors are numbered from `1` to `number_of_colors` and
the countries are numbered from `1` to `number_of_countries`. A function that
generates solutions to this problem is as follows:

```haskell
coloringSolutions :: MonadPlus m => Word -> Word -> (Word -> Word -> Bool) -> m [(Word,Word)]
coloringSolutions number_of_colors number_of_countries isAdjacentTo =
    foldM addCountryToColoring [] [1..number_of_countries]
  where
    addCountryToColoring coloring country = do
        color <- between 1 number_of_colors
        forM_ coloring $ \(other_country, other_color) ->
            when (country `isAdjacentTo` other_country) $
                guard (color /= other_color)
        return $ (country,color):coloring
```

This function works by calling `foldM` (in `Control.Monad`) which in turn calls
`addCountryToColoring` once for each country (i.e., it *folds* over the list
`[1..number_of_countries]`), carrying along the current coloring. The function
`addCountryToColoring` does the following:

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

    The first line of this snippet loops over the current coloring. The second
    line checks to see whether the current country (in the loop) is adjacent to
    the country we just colored, and if so then the third line checks that the
    two adjacent countries have different colors and fails if this is not the
    case.

3. Finally, it adds this country's color to the coloring, and returns the
   updated coloring (which will then be passed to `addCountryToColoring` at the
   next call, if any).

    ```haskell
    return $ (country,color):coloring
    ```

A major inefficiency in the code above is that a large number of the solutions
generated are equivalent in the sense that they only differ by a permutation of
the colors and the selection of the colors used (when this is less than the
total number of colors). In particular, if all `n` colors are used in a given
solution then there are `n!` equivalent solutions, if `n-1` of the `n` colors
are used then there are `n!/2` equivalent solutions, etc; in general if `m`
colors are used out of `n` for a given solution then there are `n!/(n-m)!`
equivalent solutions.

The solution to this is a trick I like to call "symmetry breaking", where you
take a symmetry (in this case, the fact that permuting the colors and/or
changing the choice of colors results in an equivalent solution) and factor it
out by forcing a particular ordering. The following code does this:

```haskell
coloringUniqueSolutions number_of_colors number_of_countries isAdjacentTo =
    liftM snd $ foldM addCountryToColoring (0,[]) [1..number_of_countries]
  where
    addCountryToColoring (number_of_colors_used,coloring) country = do
        color <- between 1 ((number_of_colors_used + 1) `min` number_of_colors)
        forM_ coloring $ \(other_country, other_color) ->
            when (country `isAdjacentTo` other_country) $
                guard (color /= other_color)
        return (number_of_colors_used `max` color,(country,color):coloring)
```

The above is a modified version of `coloringSolutions` where we *force* the
first color chosen to be 1, the second color chosen (if not the same as the
first) to be 2, and so on. Specifically, we keep track of the number of colors
used so far, and when the next color is chosen we restrict ourselves to these
colors plus the next greatest color if we have not already used all the
available colors; if our choice involves a new color then we bump up the number
of colors used for the next call to `addCountryToColoring`, otherwise we use the
same set of colors.


N-queens problem
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

(The use of `fromIntegral` comes from the fact that the input board size and
output board positions are naturally `Word`s as they cannot be negative but the
`IntSet` type only stores `Int`s, which means that we need to work internally
using `Int` and use `fromIntegral` to convert from the input `Word` and to the
output `Word`s.)

The function `go` is where most of the work happens.  For each row in the board,
it does the following:

1. First, make a non-deterministic choice from the available columns;  here we
   use the function `allFrom` (part of the `LogicGrowsOnTrees` module) to
   convert the list of available columns to a `MonadPlus` that generates it.

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

3. Finally, we recursively call `go` for the next row with updated values for
   the updated available columns and occupied diagonals, as well as the chosen
   board position added to the (partial) solution:

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

While `IntSet` is very efficient, it is even more efficient to use a bit field
for a set --- i.e., a field such that bits that are 1 correspond to being
occupied and those that are 0 correspond to being available. A solution using
this approach is as follows:

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

1. Make a non-deterministic choice within the current row for the column,
   excluding those spaces which are either occupied columns or diagonals:

    ```haskell
    column <- allFrom . goGetOpenings 0 $
        occupied_columns .|.
        occupied_negative_diagonals .|.
        occupied_positive_diagonals
    let column_bit = bit (fromIntegral column)
    ```

2. Mark the column and diagonals as being occupied, and also shift the occupied
   diagonals so that they correspond with the columns in the next row. That is,
   if a given positive diagonal intersects with column `i` for a given row then
   it intersects with column `i+1` in the next row, and if a given negative
   diagonal intersects with column `i` for a given row then it intersects with
   `i-1` in the next row. Finally, add the board position to the partial
   solution:

    ```haskell
    go (n-1)
       (row+1)
       (occupied_columns .|. column_bit)
       ((occupied_negative_diagonals .|. column_bit) `shiftR` 1)
       ((occupied_positive_diagonals .|. column_bit) `shiftL` 1)
       ((row,column):value)
   ```

The new nested function `getOpenings` scans through the input bits and builds a
list of columns where a queen may be placed without conflict.  If there are no
such columns, then the list will be empty and the code will backtrack.

It is possible to use symmetry breaking gain a significant speed-up, but the
solution I came up with that does this ended up being quite complicated.  You
can see it for yourself in the `LogicGrowsOnTrees.Examples.Queens.Advanced`
module.


Using LogicGrowsOnTrees
=======================

In the preceding part, we gave some examples of how to write a logic program
using `MonadPlus`. In this part we will talk about how to use this package to
with such programs, and in particular how to run them in parallel. First we will
show how to use this package to run a logic program serially, which is done for
two reasons: first, to introduce the functionality of this package in a simpler
setting, and second, because it is useful when one is testing a program. Next we
will show how to run a logic program in parallel using the
`LogicGrowsOnTrees.Parallel.Adapter.Threads` module. Finally, we will show how
to run a logic program in parallel using the `LogicGrowsOnTrees.Parallel.Main`
module, which provides a universal interface to all adapters as well as
automating the work required to specify a command line interface.

Note that the examples in the preceding part have all been implemented in the
`LogicGrowsOnTrees.Examples.*` modules, so in the examples to follow we will
reference them by importing them for their corresponding module rather than by
copying and pasting their source code.

Serial
------

The central data structure in `LogicGrowsOnTrees` is the `Tree` type, which, as
the name suggests, is a tree data structure where each branch corresponds to a
binary choice made in a logic program by the `mplus` function, which takes two
trees and returns a new tree whose root is a choice between the two arguments.
We have not been using this function directly because we have wanted to make
choices from a list of elements, and so we used the `allFrom` function to
essentially convert this list to a tree using `mplus`.

Because `Tree` is an instance of `MonadPlus`, all of the logic programs we have
written are automatically available in `Tree` form. Thus, to run a logic program
in serial, you can use the `exploreTree*` family of functions in
`LogicGrowsOnTree`.

The `exploreTree` function explores the entire tree and sums over all the
results; for this reason, the result type of the tree needs to be a `Monoid`.
This means that in particular if you, say, want to get a list of all solutions,
then your logic program needs to put each result in a list singleton so that the
sum builds up the list of all solutions. The following program is an example of
applying this to prints a list of all the n-queens solutions for n = 5 (also
given in `tutorial/tutorial-1.hs`):

```haskell
import LogicGrowsOnTrees (exploreTree)
import LogicGrowsOnTrees.Examples.Queens (nqueensUsingBitsSolutions)

main = print . exploreTree . fmap (:[]) . nqueensUsingBitsSolutions $ 5
```

Note the use of `fmap (:[])` to replace every result generated by the logic
program with a singleton list containing the result. When there are a lot of
results it is better to use the `Seq` type in `Data.Sequence` as it has
(amortized) asymptotically faster concatenation operations; this is done in the
following (also given in `tutorial/tutorial-2.hs`):

```haskell
import qualified Data.Sequence as Seq
import LogicGrowsOnTrees (exploreTree)
import LogicGrowsOnTrees.Examples.Queens (nqueensUsingBitsSolutions)

main = print . exploreTree . fmap Seq.singleton . nqueensUsingBitsSolutions $ 5
```

Alternatively, if you are only interested in the *number* of solutions rather
than what they are, then you should replace every solution with `WordSum 1`,
where `WordSum` is a `Monoid` (included as part of this package in the module
`LogicGrowsTrees.Utils.WordSum`) with the property that the sum of two
`WordSum`s is a `WordSum` containing the sum of the two contained values; this
is done in the following (also given in `tutorial/tutorial-3.hs`):

```haskell
import LogicGrowsOnTrees (exploreTree)
import LogicGrowsOnTrees.Utils.WordSum (WordSum(..))
import LogicGrowsOnTrees.Examples.Queens (nqueensUsingBitsSolutions)

main = print . exploreTree . fmap (const $ WordSum 1) . nqueensUsingBitsSolutions $ 5
```

Note that the only change is that we replaced `fmap Seq.singleton` with
`fmap (const $ WordSum 1)`.

If you only want the first result then you should use `exploreTreeUntilFirst`,
which return the first result found wrapped in `Just` if any results are
present, and `Nothing` if no results were found; for example, see the following
(also given in `tutorial/tutorial-4.hs`):

```haskell
import LogicGrowsOnTrees (exploreTreeUntilFirst)
import LogicGrowsOnTrees.Examples.Queens (nqueensUsingBitsSolutions)

main = print . exploreTreeUntilFirst . nqueensUsingBitsSolutions $ 10
```

Finally, if you only want a few of the results, then use
`exploreTreeUntilFound`, which takes a condition function and will stop finding
new results when it is met; the result is a pair where the first component
contains the results that were found and the second contains a `Bool` indicating
whether the condition was met. Note that the returned results might be less than
those requested if there weren't enough found to meet your condition function,
and it also might be *more* than those requested because results are not found
one at a time but rather are merged from the bottom up, meaning that there might
be a choice point where the two branches separately did not meet the condition
but their merged results did. An example of using this to find at least 3
results is illustrated as follows (also given in `tutorial/tutorial-5.hs`):

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
the second component is true. Note the condition function `((>= 3) . length)`
which computes the length of the list and checks whether it is at least three.

NOTE: If for some reason you really don't want more than, say, three solutions
--- perhaps because the solutions are very large and you never want to keep
around more than three --- then your best bet is to create a custom `Monoid`
type that, say, contains a list and never allows concatenation to let it grow
bigger than three elements.


Parallelization using the Threads module
----------------------------------------

The `LogicGrowsOnTrees.Parallel.Adapter.Threads` module provides functions that
let you run a logic program in parallel using multiple threads. You have a
couple of options for how to do this this.

First, you can use one of the many specialized functions which roughly follow
the same pattern as the `exploreTree*` functions in `LogicGrowsOnTrees`, such as
the following (also given in `tutorial/tutorial-6.hs`):

```haskell
import GHC.Conc (setNumCapabilities)

import LogicGrowsOnTrees.Parallel.Adapter.Threads
    (RunOutcome(..)
    ,TerminationReason(..)
    ,changeNumberOfWorkers
    ,exploreTree
    ,setNumberOfWorkers
    )
import LogicGrowsOnTrees.Utils.WordSum (WordSum(..))
import LogicGrowsOnTrees.Examples.Queens (nqueensUsingBitsSolutions)

main = do
    setNumCapabilities 2
    RunOutcome statistics termination_reason <-
        exploreTree (setNumberOfWorkers 2)
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

First, observe that `exploreTree` now has an additional argument,
`setNumberOfWorkers 2`. This argument is called the *controller*, and is a loop
that is run that lets you issue commands to the supervisor such as aborting,
requesting a progress update, and changing the number of workers (which can be
done at any time in the run and can even bring the number down to zero).
`setNumberOfWorkers` is a function that changes the number of workers to be
equal to its argument, spawning or killing workers as necessary.

In order for the two worker threads to run in parallel, two things need to
happen. First, you need to compile with the `-threaded` option, and second, you
need to set the number of capabilities to two so that up to two threads can run in
parallel, as is done in the first line of the body of `main`:

```haskell
setNumCapabilities 2
```

(Alternatively, you could also use the `+RTS -N#` command-line option to set the
number of capabilities to `#`.)

Now observe that in the first line of the body of the main function, we have

```haskell
RunOutcome statistics termination_reason <-
```

Now that we are running many workers in parallel, the result of the exploration
is a bit more complicated. The result type is a `RunOutcome`, which contains the
run statistics and the termination reason. The run statistics contain a lot of
information whose primary purpose is to help one diagnose why one is not getting
the appropriate speedup as the number of workers increases. The termination
reason contains information about why the run terminated. As you can see at the
end of the code, there are three possibilities:

```haskell
case termination_reason of
    Aborted progress -> putStrLn "Count aborted."
    Completed (WordSum count) -> putStrLn $ "Found " ++ show count ++ " solutions."
    Failure progress message -> putStrLn $ "Failed: " ++ message
```

First, we have `Aborted`, which means that a request was made to abort the run;
it contains the `progress` that had been made up to that point, which can be
used to resume the run at the same point later.

Second, we have `Completed`, which means that the run terminated normally; it
contains the final result in the run, which in this case is a `Word` wrapped in
a `WordSum`.

Finally, we have `Failure`, which indicates that something went horribly wrong
during the run, such as an exception being thrown; it contains both the
`progress` that had been made up to that point and also a `message` that
describes what happened. If your logic program is pure, then this most likely
means that there is a bug somewhere in your program. (If it is not pure, which
we have not covered in this tutorial, a `Failure` might just mean that, say, an
external resource that is needed by the program was not available.)

In the case of both `Aborted` and `Failure`, there is an argument which
represents the current progress of the exploration, which you can use as the
starting point by calling the `exploreTreeStartingFrom` function.

There is an important caveat, however: it only makes sense to resume an
exploration using a checkpoint *if you have not changed the program*, because in
general if you change the program, then you change the `Tree`, which means that
the checkpoint is no longer a valid; in particular, if the explored part of the
tree changes, then your current sum over results is no longer correct, and if
the shape of the tree changes, then in general the checkpoint will not line up
with it and will raise an error. (Having said this, if you make a change that
only affects parts of the tree that have not been explored, then you *might* be
fine.)

Because of this, it will rarely make sense to resume from a `Failure` if your
program is pure, because an exception will almost always signal the presence of
a bug. The main reason for including the `progress` with the `Failure` is
because, although we have not discussed this, it is possible to write logic
programs that run in the I/O monad and require access to, say, a database
server; if the database server goes down it makes perfect sense to restart it
and resume the run from the last `progress`.

In the following code, we show an example of resuming after aborting, as well as
of a non-trivial controller (also given in `tutorial/tutorial-7.hs`):

```haskell
import Control.Monad.IO.Class (liftIO)
import Data.Monoid (mempty)
import GHC.Conc (setNumCapabilities)
import System.Exit (exitFailure,exitSuccess)

import LogicGrowsOnTrees.Checkpoint (Progress(..))
import LogicGrowsOnTrees.Parallel.Adapter.Threads
    (RunOutcome(..)
    ,TerminationReason(..)
    ,abort
    ,changeNumberOfWorkers
    ,exploreTreeStartingFrom
    ,requestProgressUpdate
    ,setNumberOfWorkers
    )
import LogicGrowsOnTrees.Utils.WordSum (WordSum(..))
import LogicGrowsOnTrees.Examples.Queens (nqueensUsingBitsSolutions)

main = setNumCapabilities 2 >> go mempty
 where
  go progress@(Progress _ (WordSum count)) = do
    putStrLn $ "Counting... (starting with " ++ show count ++ " solutions); press <Enter> to abort"
    RunOutcome statistics termination_reason <-
        exploreTreeStartingFrom
            progress
            (do setNumberOfWorkers 2
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
(do setNumberOfWorkers 2
    _ <- liftIO $ getLine
    _ <- requestProgressUpdate
    abort
)
```

Whereas previously the controller just set the number of workers and quit, it
now instead waits for the user to press enter, and if the user does so, then the
controller tells the supervisor that it should perform a progress update ---
that is, that it should contact all the workers and fetch their current results
and checkpoints --- and then finally it tells the supervisor to abort.

Next, note that the `main` function is a loop:

```haskell
main = setNumCapabilities 2 >> go mempty
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

Obviously this is not a particularly useful controller, although it does
illustrate that the run can be arbitrarily aborted and restarted from a
checkpoint without suffering any problems. For an example of a more useful
controller, see the following (also given in `tutorial/tutorial-8.hs`):

```haskell
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import System.IO (hFlush,stdout)

import LogicGrowsOnTrees.Parallel.Adapter.Threads
    (RunOutcome(..)
    ,TerminationReason(..)
    ,changeNumberOfWorkers
    ,exploreTree
    ,setNumberOfWorkers
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
            setNumberOfWorkers
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
and then changes the number of workers to be equal to it, which for example you
could use to adjust the number of processors being used by the run to be larger
or smaller depending on how many processors you want to use for other tasks at
that moment.

(Unfortunately, calling `setNumCapabilities` many times in succession can
destabilize the GHC runtime, so for this example you will need to use `+RTS -N#`
on the command-line to set the number of capabilities to be equal to the largest
number of workers that you will want to run in parallel.)

As in the serial case, there are multiple modes in which an exploration can be
run.  The following code only looks for the first result (also given in
`tutorials/tutorial-9.hs`):

```haskell
import GHC.Conc (setNumCapabilities)

import LogicGrowsOnTrees.Parallel.Adapter.Threads
    (RunOutcome(..)
    ,TerminationReason(..)
    ,changeNumberOfWorkers
    ,exploreTreeUntilFirst
    ,setNumberOfWorkers
    )
import LogicGrowsOnTrees.Checkpoint (Progress(..))
import LogicGrowsOnTrees.Examples.Queens (nqueensUsingBitsSolutions)

main = do
    setNumCapabilities 2
    RunOutcome statistics termination_reason <-
        exploreTreeUntilFirst (setNumberOfWorkers 2)
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

If the run finds no solution, then it returns `Nothing`. If it does return a
solution, then it returns a `Progress` value, which contains not only the
solution but also the `checkpoint`; the reason for returning the `checkpoint` is
that it allows you to resume from it if you decide that you want to find more
solutions at some point in the future.

As in the serial case, you can also request that only some of the results be
found, as in the following code which looks for at least five solutions (also
given in `tutorials/tutorial-10.hs`):

```haskell
import GHC.Conc (setNumCapabilities)

import LogicGrowsOnTrees.Parallel.Adapter.Threads
    (RunOutcome(..)
    ,TerminationReason(..)
    ,changeNumberOfWorkers
    ,exploreTreeUntilFoundUsingPush
    ,setNumberOfWorkers
    )
import LogicGrowsOnTrees.Checkpoint (Progress(..))
import LogicGrowsOnTrees.Examples.Queens (nqueensUsingBitsSolutions)

main = do
    setNumCapabilities 2
    RunOutcome statistics termination_reason <-
        exploreTreeUntilFoundUsingPush
            ((>= 5) . length)
            (setNumberOfWorkers 2)
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
otherwise, where again the `checkpoint` allows you to resume the search at some
point in the future if you wish.

There is also an `exploreTreeUntilFoundUsingPull`, function which is similar to
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
easier;  like the main controller thread, it will be killed when the run is
over.)


Parallelization using the Main framework
----------------------------------------

`Threads` is one of the *adapters* provided by `LogicGrowsOnTrees` and its
siblings. Each of these adapters provides a way of adapting the
supervisor/worker parallelization model to a particular means of running
computations in parallel. The current adapters are as follows:

* `Threads`

    This adapter provides parallelism by spawning multiple threads; the number
    of workers can be changed arbitrarily at runtime (though you need to make
    sure that the number of capabilities is also high enough for all of them to
    run in parallel). This adapter is the only one that requires the threaded
    runtime, which adds additional overhead.

* `Processes`

    This adapter provides parallelism by spawning a child process for each
    worker;  the number of workers can be changed arbitrarily at runtime.

    Install `LogicGrowsOnTrees-processes` to use this adapter.

* Network

    This adapter provides parallelism by allowing multiple workers to connect to
    a supervisor over a network; the number of workers is then equal to the
    number that are are connected to the supervisor. (It is possible for the
    same process to be both a supervisor and one or more workers, though this is
    only really useful for testing purposes.)

    Install `LogicGrowsOnTrees-network` to use this adapter.

* `MPI`

    This adapter provides parallelism using the Message Passing Interface (MPI),
    which is the standard communication system used in supercomputers, allowing
    you to use a very large number of nodes in your run. One of the nodes (#0)
    will act entirely as the supervisor and the rest will act as workers.

    Install `LogicGrowsOnTrees-MPI` to use this adapter; note that you will need
    to have an MPI implementation installed (such as
    [OpenMPI](http://www.open-mpi.org/)).

All of these adapters provide low-level means of accessing their functionality
directly if you wish (though they are much more complicated to use than the
`exploreTree` functions in `Threads`), but there is also a universal high-level
interface that works for *all* of the adapters, which we will now discuss.

The `Main` module provides a framework that automates a lot of the work of
setting up and running an exploration in parallel, and the interface it provides
is completely agnostic about the adapter that is used; the `mainFor*` functions
all take an argument which is the `driver` of the adapter that you are using,
and so switching to a different adapter is as simple as switching the `driver`
argument.

Here is an example of using the `Main` framework (also given in
`tutorial/tutorial-11.hs`):

```haskell
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
```

This program simply calls `mainForExploreTree` with the following arguments:

1. the `driver`, which in this case was imported from `Threads`

2.  a `Term` which specifies that our program takes a single required positional
    argument for the board size:

    ```haskell
    (required $
        pos 0
            (Nothing :: Maybe Int)
            posInfo
              { posName = "BOARD_SIZE"
              , posDoc = "the size of the board"
              }
    )
    ```

    Most of the functions above are part of
    [`cmdtheline`](http://hackage.haskell.org/package/cmdtheline), an
    applicative command-line parsing library. This library was used because it
    makes it easy to compose options together; your argument value here will
    essentially be merged in with the adapter options and some generic options
    (such as the checkpointing options).

    Specifically, `pos` here is a function that takes a position, a default
    value, and a `PosInfo` data structure that contains information about the
    name of the option and a brief description of it; the result is a value of
    type `Arg (Maybe Int)`. `required` then takes this term and maps it to a
    value of type `Term Int` with the property that an error is raised if this
    positional argument is not present.

3. a `TermInfo` which specifies the name and a short description of this
   program:
    
    ```haskell
            (defTI
                { termName = "tutorial-11"
                , termDoc = "count the number of n-queens solutions for a given board size"
                }
            )
    ```

4. an action to be executed with the final result:

    ```haskell
    (\board_size (RunOutcome _ termination_reason) -> do
        case termination_reason of
            Aborted _ -> error "search aborted"
            Completed (WordSum count) -> putStrLn $ show count ++ " solutions found for board size " ++ show board_size
            Failure _ message -> error $ "error: " ++ message
    )
    ```

    The first argument to this function is equal to the value supplied by the
    user for the first command line argument.

    NOTE: When `Completed`, any existing checkpoint file will be deleted after
    this function returns *unless* an exception is thrown, in which case it is
    kept around.

5. a function that constructs the logic program:

    ```haskell
    (fmap (const $ WordSum 1) . nqueensUsingBitsSolutions . fromIntegral)
    ```

    The argument to this function is equal to the value supplied by the user
    for the first command line argument.

This program comes with an automatically generated help screen (via. `--help`),
and it already includes options to specify the location of the checkpoint file
(if it exists, then the run will be resumed from it), how often a checkpoint
should be written, at what level to print logging messages, and whether various
server statistics should be printed to the screen (possibly useful if your
computation is not scaling well). Because we are using the `Threads` driver,
there will also be a `-n` option to set the number of threads.

If this interface seems complex, it may help to understand that part of the
reason for its complexity is that the supervisor and worker will in general be
in different processes, which means that configuration information needs to be
sent to the worker processes. The `driver` automates the mechanism for this.

Finally, it is worth noting that all that it takes to use multiple processes
instead of multiple threads is to install `LogicGrowsOnTrees-processes` and then
replace `Threads` with `Processes` in the imports.

For a slightly more sophisticated example, consider the following (also given in
`tutorial/tutorial-12.hs`):

```haskell
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
```

This differs from the previous example in two main respects: first, it prints
out solutions rather than just their count, and second there is a second
argument to the program that specifies how many should be found.

To understand what is going on, let us first look at the third argument to
`mainForExploreTreeUntilFoundUsingPush`:

```haskell
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
```

This argument essentially takes the argument from the previous example,
duplicates it to create a second argument (the number of solutions to find), and
then merges the two terms together in `Applicative` style via a call to
`liftA2`. After parsing is complete, the result will be a pair where the first
value is the board size and the second value is the number of solutions to find.

Now we look a the first argument to `mainForExploreTreeUntilFoundUsingPush`:

```haskell
(\(board_size,number_to_find) -> (>= number_to_find) . length)
```

This argument is a function that takes the configuration information and returns
a condition function that indicates where enough results have been accumulated.
In this case, the condition function checks whether the number of solutions
found (obtained via. `length`) is at least as many as were requested
(`number_to_find`).  Note that `board_size` is ignored and would probably
normally be replaced by `_` (or possibly by use of higher-order functions to
make the expression entirely point-free);  we include it here purely for
pedagogical reasons.

Next we look at the last argument of `mainForExploreTreeUntilFoundUsingPush`:

```haskell
(fmap (:[]) . nqueensUsingBitsSolutions . fromIntegral . fst)
```

This is just like in the previous example, save that now at the end there is
`fst`, which takes the first value in the configuration pair (which is the board
size) and instead of replacing each solution with a `WordSum`, it turns it into
a singleton list.

Finally, we look at the second-to-last argument:

```haskell
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
```

The difference compared to the previous example is that there are two cases for
a `Completed` run.  In the first case, the run fully completed before it was
able to find all of the requested number of solutions;  the solutions it did
find are returned in a `Left` value.  In the second case, the run found all of
the requested solutions and then stopped;  the result is a `Progress` value
whose `checkpoint` value allows you to resume the search later to find more
solutions if you wish and whose result value is the requested solutions.


Conclusion
==========

At this point you have learned how to write logic programs and how to use this
package to write them in parallel. For more information, see
[USERS_GUIDE.md](USERS_GUIDE.md) for a more detailed discussion of some aspects
of this package as well as the package haddock documentation for reference.
