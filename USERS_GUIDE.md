Introduction
============

LogicGrowsOnTrees revolves around the Tree datatype, which is an instance of
MonadPlus with the additional ability for one to cache values that are expensive
to compute (as resuming from a checkpoint and parallelizing both can cause paths
in the tree to be explored multiple times); see the documentation for
MonadExplorable for more information on this.

In the sections to follow we will present the various functionality in this
package for exploring trees.  First we will discuss how to explore trees
serially, and then we will discuss how to explore them in parallel;  for the
parallel case there are multiple options that will be discussed separately.
Finally, we will discuss how to create new adapters.


Serial exploration
==================

If you want to explore a tree serially (and without using checkpointing) then
you can use the exploreTree family of functions in the LogicGrowsOnTrees module.
There are 6 functions, which is the result of there being three different
exploration modes each of which in turn has with pure and impure variants.

The default exploration mode, "All", explores the entire tree and sums over
all of the results at the leaves of the tree; to use this mode your Tree needs
to generate results that are an instance of the Monoid class. The reason for
this is that sometimes you want to do things like counting the number of
solutions rather than generating the list of them, and it is much more efficient
to be able to do this directly rather than to generate a (possibly very large)
list which you only use by taking its length. If you do want to generate a list
of solutions then you can use `fmap (:[])` on the Tree to turn each result into
a singleton list, though if you have a large number of results then you should
create singletons of the Seq type in Data.Sequence as this type has (amortized)
asymptotically faster concatenation operations.

The second mode, "First", explores the tree until it has found the first result,
at which point it returns this result wrapped in Just;  if no result is found,
then it returns Nothing.  Note that in this case the result does not have to be
an instance of Monoid as there is no sum being performed.

The third mode, "Found", explores the tree summing over results until a given
condition function is satisfied; like "All" mode, the results have to be an
instance of Monoid. The returned value is all the results that were found as
well as a flag indicating whether the condition function was ever satisfied.

For each of the modes above, there is a variant for when you have a pure Tree,
and a variant for when have a TreeT, which is monad transformer;  in the latter
case the exploration functions will return their value inside the monad nested
in the TreeT.


Parallel exploration
====================

Although this package can be used to explore trees serially, it really shines
when you want to perform an exploration in parallel. In the first subsection we
will describe the worker/supervisor model used by this package for
parallelization. Following that, we will briefly cover the various parallel
exploration modes and tree purities.  Next, we will discuss the various adapters
that are available, and in particular how to use the Threads adapter.  Finally,
we will discuss how to use the Main infrastructure, including run outcomes.


Supervisors and workers
-----------------------

LogicGrowsOnTrees uses a supervisor/worker model for parallelization.  That is,
at any given time there is a supervisor that keeps track of the global state of
the exploration and zero or more workers that are exploring the tree in
parallel --- zero is a valid number because some adapters allow for the number
of workers to change during the run.

The supervisor functionality is given in the
LogicGrowsOnTrees.Parallel.Common.Supervisor module. It is not intended to
be used directly, but rather you will be using a provided adapter which builds
on top of this an provides a simplified, specialized interface.  This package
includes the Threads adapter (parallelism via. threads); other packages include
a Processes adapter (parallelism via. processes), a Network adapter (parallelism
via. zero or more processes connecting over the network to the supervisor), and
an MPI (Message Passing Interface) adapter.

The worker functionality is given in the
LogicGrowsOnTrees.Parallel.Common.Worker module.  As with the Supervisor module,


Exploration Modes
-----------------

The modes in which a parallel exploration can be run are given by the
ExplorationMode type in the LogicGrowsOnTrees.Parallel.Common.ExplorationMode
module. Many of the functions in the parallelization infrastructure come in
specialized families where there is a function for each mode (and purity), and
if you use these then you might never need to deal with ExplorationMode
directly; nonetheless it is good to know what values it can take anyway as the
constructor names are used as suffixes in specialized functions.

The first mode is AllMode; functions specialized to this mode have no suffix. It
acts just like the All mode discussed in the serial exploration section, i.e. it
sums over all results (which must therefore be an instance of Monoid).

The second mode is FirstMode; functions specialized to this mode have the suffix
UntilFirst. It acts just like the First mode discussed in the serial exploration
section, i.e. the exploration terminates when the first result has been found
(which need not be an instance of Monoid).

The third mode is FoundModeUsingPull; functions specialized to this mode have
the suffix UntilFoundUsingPull. It acts as the Found mode discussed in the
serial exploration section, i.e. it sums over all results found until a criteria
specified by the argument to the FoundModeUsingPull constructor is satisfied.
All found results are kept locally at the various workers running in parallel
and only merged when the supervisor sends out a global progress update request
that "pulls" all of the results in the system to it. Because of this, it is
possible that the workers will have collectively found enough results to satisfy
the criteria, but the system as a whole will not know this until a progress
update has been performed; for this reason, if you are using this mode then you
need to perform a progress update on a regular basis.

The last mode is FoundModeUsingPush; functions specialized to this mode have
the suffix UntilFoundUsingPush.  This mode functions exactly like
FoundModeUsingPull except that every result found is sent straight to the
supervisor, which means that the very instant that the system finds the desired
results it will know about it immediately.  The potential downside to this mode
is that there is a small amount of overhead incurred in sending the result to
the supervisor, and so if there are a large number of results it might be more
efficient to accumulate them locally with the occasional pull rather than to
send each result to the supervisor as it is found.


Purity
------

The Purity of a tree indicates whether it is Pure or ImpureAtopIO. If a tree is
Pure then it has no side effects (more precisely, the nested monad is the
Identity). If a tree is ImpureAtopIO, then it has side-effects, and futhermore
it has IO as the base monad in the stack, where this latter restriction comes
from the fact that the worker needs to run in a monad that has IO at the base;
ImpureAtopIO takes a single parameter that indicates how to run the given action
in the IO monad.

The families of specialized functions actually have *three* cases:  Pure,
Impure, and IO, where IO is a special case of Impure provided for convenience.
The functions accepting an impure tree also have an additional parameter to
specify how to run it in the IO monad.


Adapters
--------

An adapter module provides a way of adapting the supervisor/worker
parallelization model to a particular means of running computations in parallel.
The current adapters are as follows:

* Threads

    This adapter provides parallelism by spawning multiple threads; the number
    of workers can be changed arbitrarily at runtime (though you need to make
    sure that the number of capabilities is also high enough for all of them to
    run in parallel). This back-end is the only one that requires the threaded
    runtime, which adds additional overhead.

* Processes

    This adapter privides parallelism by spawning a child process for each
    worker;  the number of workers can be changed arbitrarily at runtime.

    Install `LogicGrowsOnTrees-processes` to use this adapter.

* Network

    This adapter provides parallelism by allowing multiple workers to connect to
    a supervisor over a network; the number of workers is then equal to the
    number that are are connected to the supervisor. (It is possible for the
    same process to be both a supervisor and one or more workers, though this is
    only useful for testing.)

    Install `LogicGrowsOnTrees-network` to use this adapter.

* MPI

    This adapter provides parallelism using the Message Passing Interface (MPI)
    which is the standard communication system used in supercomputers, allowing
    you to use a very large number of nodes in your run. One of the nodes (id 0)
    will act entirely as the supervisor and the rest will act as workers.

    Install `LogicGrowsOnTrees-MPI` to use this adapter;  note that you will
    need to have an MPI implementation installed.

All of these modules offer 'low-level' interfaces that are more complicated to
use but which give you more control. To use these interfaces, look for functions
named `runExplorer`, `runSupervisor`, and `runWorker`; not all of these will be
present for every adapter.

Through the Main framework the above also offer high-level interface, but before
we discuss this it is worth discussing the Threads module which offers a
higher-level direct interface than the others.


Threads
-------

The Threads adapter offers a higher-level interface than the others, mainly
because it does everything within a single process and so does not have to worry
about things like whether the current process is worker of the supervisor. In
the Threads module, there are `3 x 4 x 2 = 24` specialized functions, which have
the naming convention exploreTreeXYZ where:

* X is empty for 'AllMode' and takes the form 'UntilM' for any other mode M;

* Y is empty for Pure trees, IO for trees running in IO, and Impure for general
  impure trees; and

* Z is 'StartingFrom' if starting from an initial progress and empty otherwise.

All of these functions take a 'controller', which is a function that has the
ability to make requests to the supervisor. You would use this, for example, if
you wanted to write a checkpoint on a regular basis; specifically, you would do
this by setting up a timer that on a regular basis calls
`requestProgressUpdateAsync`, which tells the supervisor to request a progress
update from all active workers and then returns the current progress, and then
write this progress to a file. All of the functions you can call in the explorer
are exported by the Threads, and they include the ability to fork new threads
which can make life easier.


Main
----

The Main framework is designed to make your life easier by automating things
like checkpointing. It provides a universal interface to all adapters though a
driver system whereby all of the 'mainForExploreTree' functions have a 'driver'
parameter which you import from the adapter that you want to use.  This is the
*only* parameter that depends on the adapter, so if you want to switch to using
a different adapter you only need to change your adapter module import so that
the driver is pulled from the desired adapter.

The main functions that you will be interested in are the `mainForExploreTreeXY`
methods where X corresponds to the purity (empty for Pure) and Y corresponds to
the exploration mode (empty for all).  Unlike threads, there is not argument to
specify a starting progress because instead this will be derived from a
command-line option specifying the checkpoint file;  if it exists, then it is
used as the starting point.

All of these functions also take a `Term` argument and a `TermInfo` argument.
The latter just specifies a brief name and description for your program; it
should suffice to glance at TUTORIAL.md to see what to do. The former specifies
the command-line arguments and/or options that your particular logic program
needs to, for example, specify the size of the problem (such as the board size
in the n-queens problem); for simple cases it should again suffice to glance at
the tutorials, but in general you may need to learn how to use Cmdtheline. The
reason why Cmdtheline is used is because it provides a means of combining
arguments and options from several sources, which in this case includes the
configuration for your logic program, configuration for the adapter (such as the
number of threads or processes), and configuration that is used by the Main
module itself (such as the checkpoint file and frequency, and whether to print
various server statistics). The compiled program will also have a nice `--help`
page. After the command lines have been parsed, the configuration information
for the term you provided will be passed as an argument to the functions that
you provide in other arguments.  (Again, see the tutorial for examples of this.)


Outcomes
--------

Once the run has finished, a RunOutcome will be returned that is one of the
following three posibilities:

* `Aborted` means that a request was made to abort the run.

* `Completed` means that the run terminated normally.

* `Failure` means that an unexpected error occurred.

It is worth noting that `Completed` doesn't necessarily mean that the run was
successful; if you asked for the first solution and no solutions were found,
then the result will be `Nothing`. Likewise if you (using one of the Found
modes) ask for k results but fewer than k were found, then the result will be
`Left` with the results that were found. If only one or a few results are
requested and they were all found, then the result will also have the progress
so you can resume the run at a later time to find more results. If using Main,
then note that the checkpoint file specified by the user will be deleted after
you are finished processing the outcome, so at this time if you want to save
this progress you will need to do so separately.

NOTE:  You should almost never resume from a checkpoint if you change the tree!
       This is only safe if the only parts of the tree that have been changed
       are those that have not yet been explored.  If you do change parts of the
       tree that have been explored, then if you are lucky an exception will be
       thrown (if the branching structure has changed) and if you are unlucky
       then your results will be silently corrupted.
