What is this thing?
===================

LogicGrowsOnTrees is a library that lets you use a standard Haskell domain
specific language (MonadPlus and friends) to write logic programs (by which we
mean programs that make non-deterministic choices and have guards to enforce
constraints) that you can run in a distributed setting.


Could you say that again in Haskellese?
=======================================

LogicGrowsOnTrees provides a logic programming monad designed for distributed
computing;  specifically, it takes a logic program (written using MonadPlus),
represents it as a (lazily generated) tree, and then explores the tree in
parallel.


What do you mean by "distributed"?
==================================

By "distributed" I mean parallelization that does not required shared memory but
only some form of communication. In particular there is a sibling package to
this on that provides an "adapter" for MPI, which gives you immediate access to
large numbers of nodes on most supercomputers. In fact, the following is the
result of a scaling experiment to see how well the time needed to solve the
N-Queens problem for N=17, N=18, and N=19 on a local cluster:

![Alt text](scaling/scaling.png "Scaling experiment")

The above was obtained by running a job three times for each number of workers
and problem size and then taking the best of three (*); the maximum number of
workers for this experiment was limited by the size of the cluster.

(*) The other two data points were usally within small percentage of the other
two points, save for the leftmost datapoint for each problem size which varied
from 150%-200% of the orignal; the full data set is available in the `scaling/`
directory.


When would I want to use this?
==============================

This package is useful when you have a large space of solutions that you want to
explore in parallel to satisfy some goal --- such as finding all solutions,
counting all solutions, finding just one or a few solutions, etc. --- and this
space can furthermore be defined efficiently using a logic program.

LogicGrowsOnTrees is particularly useful when your solution space has a lot of
structure as it gives you full control over the non-deterministic choices that
are made, which lets you avoid generating entire decision trees that can only
end in failure, as well as letting you factor out symmetries so that only one
solution is generated out of some equivalence class. For example, if
permutations result in equivalnt solutions then you can factor out this symmetry
by only choosing later parts of a potential solution that are greater than
earlier parts of the solution.


Where can I learn more about this?
==================================

Read [TUTORIAL.md](TUTORIAL.md) for a tutorial of how to write and run logic
programs using this package, [USERS_GUIDE.md](USERS_GUIDE.md) for a more
detailed explanation of how things work, and the haddock documentation available
at:

What platforms does it support:
===============================

The following packages have been tested on Linux, OSX, and Windows using the
latest Haskell Platform (2013.2.0.0):

* LogicGrowsOnTrees (+ threads adapter);

* LogicGrowsOnTrees-processors; and

* LogicGrowsOnTrees-network.

LogicGrowsOnTrees-MPI has been tested as working on Linux and OSX using OpenMPI,
and since it only uses very basic functionality (just sending, probing, and
receiving messages) it should work on any MPI implementation.

(I wasn't able to try Microsoft's MPI because it only let me install the 64-bit
version (as my test machine was 64-bit) but Haskell on Windows is only 32-bit.)


Why would I use this instead of Cloud Haskell?
==============================================

This package is higher level than Cloud Haskell in that it takes care of all the
work of parallelizing your logic program for you. In fact, if one wished one
could potentially write an "adapter" for LogicGrowsOnTrees that lets one use
Cloud Haskell as the communication layer.


Why would I use this instead of MapReduce?
==========================================

MapReduce and LogicGrowsOnTrees can both be viewed (in a *very* rough sense) as
mapping a function over a large data set and then to perform a reduction on it.
The primary difference between them is that MapReduce is optimized for the case
where you have a huge data set that already exists, whereas LogicGrowsOnTrees is
optimized for the case where your data set needs to be generated on the fly
using a (possibly quite expensive) operation that involves making many
non-deterministic choices some of which lead to dead-ends (that produce no
results). Having said that, LogicGrowsOnTrees can also be used like MapReduce by
having your function generate data by reading it from files or possibly from a
database.


Why would I use this instead of a SAT/SMT/CLP/etc. solver?
==========================================================

First, it should be mentioned that one could use LogicGrowsOnTrees to implement
these solvers. That is, a solver could be written that uses the `mplus` function
whenever it needs to make a non-derministic choices (e.g. when guessing whether
a boolean variable should be true or false) and `mzero` to indicate failure
(e.g., when it has become clear that a particular set of choices cannot result
in a valid solution), and then it gets to use the parallelization framework of
this package for free! (For an example of such a solver, see the
[incremental-sat-solver
package](http://hackage.haskell.org/packages/archive/incremental-sat-solver/0.1.7/doc/html/Data-Boolean-SatSolver.html)
(which was note written by me).

Having said that, if your problem can most easily and efficiently be expressed
as an input to a specialized solver, then this package might not be as useful to
you. *However*, even in this case you might still want to consider using this
package if there are constraints that you cannot express easily or efficiently
using one of the specialized solvers because this package gives you complete
control over how choices are made which means that you can, for example, enforce
a contraint by only making choices that are guaranteed to satisfy it, rather than
generating choices that may or may not satisfy it and then having to filter out
all solutions that don't.


Why Haskell?
============

Haskell has many strengths that made it ideal for this project:

1. Laziness

    Haskell has lazy (*) evaluation which means that it does not evaluate
    anything until the value is required to make progress; this capability means
    that functions can be used as control structures. In particular, when you
    use `mplus a b` to signal a non-derministic choice, neither `a` nor `b` will
    be evaluated unless one chooses to explore respectively the left and/or
    right branch of the corresponding decision tree. This is very powerful
    because it allows us to explore the decision tree of a logic program as much
    or as little as we want and only have to pay for the parts that we choose to
    explore.

    (*) Technically Haskell is "non-strict" rather than "lazy", which means
    there might be times in practice when it evaluates something more than is
    strictly needed.


2. Purity

    Haskell is a pure language, which means that functions have no (observable)
    side-effects other than returning a value (*); in particular this means that
    all operations on data must be immutable, which means that they must result
    in a new value (that may reference parts or even all of the old value)
    rather than modifying the old value. This is an incredibly boon because it
    means that when we backtrack up a decision tree to explore another branch we
    do not have to carry out some kind of undo operation to restore the old
    values from the new values because the old values were never lost! All that
    you have to do is "forget" about the new values and you are done.
    Furthermore, most data structures in Haskell are designed to have efficient
    immutable operations which try to re-use as much of an old value as possible
    in order to minimize the amount of copying needed to construct the new
    value.

    (Having said all of this, although it is strongly recommended that your
    logic program be pure by making it have type `Tree` as this will cause the
    type system to enforce purity, you can add various kinds of side-effects by
    using type `TreeT` instead; an example of a time when it might make sense to
    do this is if there is a data set that will be constant over the run which
    is large enough that you want to read it in from various files or a database
    as you need it. In general if you use side-effects then they need to be
    non-observable, which means that they are not affected by in which order the
    tree is explored or whether particular parts of the tree are explored more
    than once.)

    (*) Side-effects are implemented by, roughly speaking, having some values
    represent actions that cause side-effects when executed.

3. Powerful static type system

    When writing a very complicated program you want as much help as possible in
    making it correct, and Haskell's powerful type system helps you a lot here
    by harnessing the power of static analysis to ensure that all of the parts
    fit together correctly and to enforce invariants that you have encode in the
    type system.


I have more questions!
======================

Then please contact the author (Gregory Crosswhite) at gcrosswhite@gmail.com! :-)
