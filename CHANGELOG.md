version 1.1.0.2
===============

* Tweaked the test suite to resolve minor problems when running on Windows and
  older GHCs.

* Added the documentation files to the Hackage source distribution.

* Deleted some no longer relevant advice on building the tests (as the version
  dependency of test-framework-quickcheck was finally bumped).


version 1.1.0.1
===============

* Bumped lens version dependency.


Version 1.1
===========

Highlights
----------

* Many performance enhancements, speeding up code using `Main` and `Threads` by
  a factor of two and reducing the overhead of `LogicGrowsOnTrees` overall by a
  factor of two.


New Features
------------

* Now statistics can be logged on a regular basis.

* Exposed `getCurentStatistics` in the `RequestQueueMonad`, allowing one to
  obtain the statistics at any time during the run.

* Added a system for estimating the total number of CPU-hours used (including
  the time spent waiting for a workload) in total by all of the workers during
  the run.

* Made the types `Arity` and `ArityAndDepth` serializable.


Miscellaneous
-------------

* Revamped the command line options for specifying which statistics should be
  displayed in order to make them easier to use.

* Tweaked the log levels of some of the logged messages.

* Bumped version dependencies.

* Now `Context` is a list rather than a `Seq`.  (This change is what caused the
  bump to version 1.1 to conform with the PVP.)


Attempted Ideas That Turned Out To Be Bad
-----------------------------------------

* Converting from `operational` to `free` led to a performance regression.
