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
exploration modes and tree purities. Next, we will show how to run a parallel
exploration using threads. Finally, we will discuss how to use the Main
framework to create a program that explores a tree that is agnostic as to the
mechanism of parallelism (i.e., threads, processes, network, or MPI).


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
has IO as the base monad in the stack, where this latter restriction comes from
the fact that the worker needs to run in a monad that has IO at the base;
ImpureAtopIO takes a single parameter that indicates how to run the given action
in the IO monad.

The families of specialized functions actually have *three* cases:  Pure,
Impure, and IO, where IO is a special case of Impure provided for convenience.
The functions accepting an impure tree also have an additional parameter to
specify how to run it in the IO monad.
