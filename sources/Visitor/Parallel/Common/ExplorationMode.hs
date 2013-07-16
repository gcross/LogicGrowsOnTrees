{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

{-| There are several tasks for which a user may wish to use the Visitor
    package, such as gathering up all the results in a tree and stopping as soon
    as the first result is found.  Because almost all of the infrastructure
    required for these different modes is the same, rather than creating a
    different system for each mode we instead re-use the same system but pass
    around a mode parameter that dictates its behavior at various points as well
    as some of the types in the system.

    The implementation of the 'ExplorationMode' uses a GADT where each constructor
    causes 'ExplorationMode' to have a different type parameter;  this was done so
    that pattern matching can be used when the specific mode matters and also
    type families can be used to specialized types depending on the constructor.
 -}
module Visitor.Parallel.Common.ExplorationMode
    (
    -- * Types
      ExplorationMode(..)
    , AllMode(..)
    , FirstMode(..)
    , FoundModeUsingPull(..)
    , FoundModeUsingPush(..)
    -- * Type-classes
    , HasExplorationMode(..)
    -- * Type families
    , ResultFor
    , ProgressFor
    , FinalResultFor
    , WorkerIntermediateValueFor
    , WorkerFinalProgressFor
    -- * Functions
    , checkpointFromIntermediateProgress
    , initialProgress
    , initialWorkerIntermediateValue
    ) where


import Data.Monoid

import Visitor.Checkpoint

--------------------------------------------------------------------------------
------------------------------------ Types -------------------------------------
--------------------------------------------------------------------------------

{-| A type indicating the mode of the exploration.  Note that this is a GADT for
    which the type parameter is unique to each constructor in order to allow
    associated types to be specialized based on the value.

    Unfortunately Haddock does not seem to support documenting the constructors
    of a GADT, so for the documentation for each constructor refer to the types
    used to tag them, defined after the 'ExplorationMode' type.
 -}
data ExplorationMode exploration_mode where
    AllMode :: Monoid result ⇒ ExplorationMode (AllMode result)
    FirstMode :: ExplorationMode (FirstMode result)
    FoundModeUsingPull :: Monoid result ⇒ (result → Maybe final_result) → ExplorationMode (FoundModeUsingPull result final_result)
    FoundModeUsingPush :: Monoid result ⇒ (result → Maybe final_result) → ExplorationMode (FoundModeUsingPush result final_result)

{-| Explore all nodes and sum the results in all of the leaves. -}
data AllMode result
{-| Explore nodes until a result is found, and if so then stop. -}
data FirstMode result
{-| Explore nodes, summing their results, until a condition has been met, and if
    so stop and return the condition's result; `Pull` means that each worker's
    results will be kept and summed locally until a request for them has been
    received from the supervisor, which means that there might be a period of
    time where the collectively found results meet the condition but the system
    is unaware of this as they are scattered amongst the workerss.
 -}
data FoundModeUsingPull result final_result
{-| Same as 'FoundModeUsingPull', but pushes each result to the supervisor as it
    is found rather than summing them in the worker until they are requested by
    the supervisor, which guarantees that the system will recognize when the
    condition has been met as soon as final result needed was found but has the
    downside that if there are a large number of results needed then sending
    each one could be much more costly then summing them locally and sending the
    current total on a regular basis to the supervisor.
 -}
data FoundModeUsingPush result final_result

--------------------------------------------------------------------------------
--------------------------------- Type-classes ---------------------------------
--------------------------------------------------------------------------------

{-| This class indicates that a monad has information about the current
    exploration mode tag type that can be extracted from it.
 -}
class HasExplorationMode (monad :: * → *) where
    type ExplorationModeFor monad :: *

--------------------------------------------------------------------------------
-------------------------------- Type families ---------------------------------
--------------------------------------------------------------------------------

{- $families
The type families in this section allow the types to be used at various places
to be specialized based on the current exploration mode.
-}

{-| The result type of the tree. -}
type family ResultFor exploration_mode :: *
type instance ResultFor (AllMode result) = result
type instance ResultFor (FirstMode result) = result
type instance ResultFor (FoundModeUsingPull result final_result) = result
type instance ResultFor (FoundModeUsingPush result final_result) = result

{-| The type of progress, which keeps track of how much of the tree has already
    been explored.
 -}
type family ProgressFor exploration_mode :: *
type instance ProgressFor (AllMode result) = Progress result
type instance ProgressFor (FirstMode result) = Checkpoint
type instance ProgressFor (FoundModeUsingPull result final_result) = Progress result
type instance ProgressFor (FoundModeUsingPush result final_result) = Progress result

{-| The final result of exploring the tree. -}
type family FinalResultFor exploration_mode :: *
type instance FinalResultFor (AllMode result) = result
type instance FinalResultFor (FirstMode result) = Maybe (Progress result)
type instance FinalResultFor (FoundModeUsingPull result final_result) = Either result (Progress (final_result,result))
type instance FinalResultFor (FoundModeUsingPush result final_result) = Either result (Progress final_result)

{-| The intermediate value being maintained internally by the worker. -}
type family WorkerIntermediateValueFor exploration_mode :: *
type instance WorkerIntermediateValueFor (AllMode result) = result
type instance WorkerIntermediateValueFor (FirstMode result) = ()
type instance WorkerIntermediateValueFor (FoundModeUsingPull result final_result) = result
type instance WorkerIntermediateValueFor (FoundModeUsingPush result final_result) = ()

{-| The final progress returned by a worker that has finished. -}
type family WorkerFinalProgressFor exploration_mode :: *
type instance WorkerFinalProgressFor (AllMode result) = Progress result
type instance WorkerFinalProgressFor (FirstMode result) = Progress (Maybe result)
type instance WorkerFinalProgressFor (FoundModeUsingPull result final_result) = Progress (Either result final_result)
type instance WorkerFinalProgressFor (FoundModeUsingPush result final_result) = Progress result

--------------------------------------------------------------------------------
---------------------------------- Functions -----------------------------------
--------------------------------------------------------------------------------

{-| Extracts the 'Checkpoint' component from a progress value. -}
checkpointFromIntermediateProgress ::
    ExplorationMode exploration_mode →
    ProgressFor exploration_mode →
    Checkpoint
checkpointFromIntermediateProgress AllMode = progressCheckpoint
checkpointFromIntermediateProgress FirstMode = id
checkpointFromIntermediateProgress (FoundModeUsingPull _) = progressCheckpoint
checkpointFromIntermediateProgress (FoundModeUsingPush _) = progressCheckpoint

{-| The initial progress at the start of the exploration. -}
initialProgress :: ExplorationMode exploration_mode → ProgressFor exploration_mode
initialProgress AllMode = mempty
initialProgress FirstMode = mempty
initialProgress (FoundModeUsingPull _) = mempty
initialProgress (FoundModeUsingPush _) = mempty

{-| The initial intermediate value for the worker. -}
initialWorkerIntermediateValue ::
    ExplorationMode exploration_mode →
    WorkerIntermediateValueFor exploration_mode
initialWorkerIntermediateValue AllMode = mempty
initialWorkerIntermediateValue FirstMode = ()
initialWorkerIntermediateValue (FoundModeUsingPull _) = mempty
initialWorkerIntermediateValue (FoundModeUsingPush _) = ()
