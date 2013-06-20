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

    The implementation of the 'VisitorMode' uses a GADT where each constructor
    causes 'VisitorMode' to have a different type parameter;  this was done so
    that pattern matching can be used when the specific mode matters and also
    type families can be used to specialized types depending on the constructor.
 -}
module Visitor.Parallel.Common.VisitorMode
    (
    -- * Types
      VisitorMode(..)
    , AllMode(..)
    , FirstMode(..)
    , FoundModeUsingPull(..)
    , FoundModeUsingPush(..)
    -- * Type-classes
    , HasVisitorMode(..)
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

{-| A type indicating the mode in which the visiting is being run.  Note that
    this is a GADT for which the type parameter is unique to each constructor
    in order to allow types to be specialized.

    Unfortunately Haddock does not seem to support documenting the constructors
    of a GADT, so for the documentation for each constructor refer to the types
    used to tag them, defined after the 'VisitorMode' type.
 -}
data VisitorMode visitor_mode where
    AllMode :: Monoid result ⇒ VisitorMode (AllMode result)
    FirstMode :: VisitorMode (FirstMode result)
    FoundModeUsingPull :: Monoid result ⇒ (result → Maybe final_result) → VisitorMode (FoundModeUsingPull result final_result)
    FoundModeUsingPush :: Monoid result ⇒ (result → Maybe final_result) → VisitorMode (FoundModeUsingPush result final_result)

{-| Visit all nodes and sum the results in all of the leaves. -}
data AllMode result
{-| Visit nodes until a result is found, and if so then stop. -}
data FirstMode result
{-| Visit nodes, summing their results, until a condition has been met, and if
    so stop and return the condition's result; "Pull" means that each worker's
    results will be kept and summed locally until a request for them has been
    received from the supervisor.
 -}
data FoundModeUsingPull result final_result
{-| Same as 'FoundModeUsingPull', but pushes each result to thesupervisor as it
    is found rather than summing them in the worker until they are requested by
    the supervisor.
 -}
data FoundModeUsingPush result final_result

--------------------------------------------------------------------------------
--------------------------------- Type-classes ---------------------------------
--------------------------------------------------------------------------------

{-| This class indicates that a monad has information about the current visitor
    mode tag type that can be extracted from it.
 -}
class HasVisitorMode (monad :: * → *) where
    type VisitorModeFor monad :: *

--------------------------------------------------------------------------------
-------------------------------- Type families ---------------------------------
--------------------------------------------------------------------------------

{- $families
The type families in this section allow the types to be used at various places
to be specialized based on the current visitor mode.
-}

{-| The result type of the tree generator. -}
type family ResultFor visitor_mode :: *
type instance ResultFor (AllMode result) = result
type instance ResultFor (FirstMode result) = result
type instance ResultFor (FoundModeUsingPull result final_result) = result
type instance ResultFor (FoundModeUsingPush result final_result) = result

{-| The type of progress, which keeps track of how much of the tree has already
    been visited.
 -}
type family ProgressFor visitor_mode :: *
type instance ProgressFor (AllMode result) = Progress result
type instance ProgressFor (FirstMode result) = Checkpoint
type instance ProgressFor (FoundModeUsingPull result final_result) = Progress result
type instance ProgressFor (FoundModeUsingPush result final_result) = Progress result

{-| The final result of visiting the tree. -}
type family FinalResultFor visitor_mode :: *
type instance FinalResultFor (AllMode result) = result
type instance FinalResultFor (FirstMode result) = Maybe (Progress result)
type instance FinalResultFor (FoundModeUsingPull result final_result) = Either result (Progress (final_result,result))
type instance FinalResultFor (FoundModeUsingPush result final_result) = Either result (Progress final_result)

{-| The intermediate value being maintained internally by the worker. -}
type family WorkerIntermediateValueFor visitor_mode :: *
type instance WorkerIntermediateValueFor (AllMode result) = result
type instance WorkerIntermediateValueFor (FirstMode result) = ()
type instance WorkerIntermediateValueFor (FoundModeUsingPull result final_result) = result
type instance WorkerIntermediateValueFor (FoundModeUsingPush result final_result) = ()

{-| The final progress returned by a worker that has finished. -}
type family WorkerFinalProgressFor visitor_mode :: *
type instance WorkerFinalProgressFor (AllMode result) = Progress result
type instance WorkerFinalProgressFor (FirstMode result) = Progress (Maybe result)
type instance WorkerFinalProgressFor (FoundModeUsingPull result final_result) = Progress (Either result final_result)
type instance WorkerFinalProgressFor (FoundModeUsingPush result final_result) = Progress result

--------------------------------------------------------------------------------
---------------------------------- Functions -----------------------------------
--------------------------------------------------------------------------------

{-| Extracts the 'Checkpoint' component from a progress value. -}
checkpointFromIntermediateProgress ::
    VisitorMode visitor_mode →
    ProgressFor visitor_mode →
    Checkpoint
checkpointFromIntermediateProgress AllMode = progressCheckpoint
checkpointFromIntermediateProgress FirstMode = id
checkpointFromIntermediateProgress (FoundModeUsingPull _) = progressCheckpoint
checkpointFromIntermediateProgress (FoundModeUsingPush _) = progressCheckpoint

{-| The initial progress at the start of the visiting. -}
initialProgress :: VisitorMode visitor_mode → ProgressFor visitor_mode
initialProgress AllMode = mempty
initialProgress FirstMode = mempty
initialProgress (FoundModeUsingPull _) = mempty
initialProgress (FoundModeUsingPush _) = mempty

{-| The initial intermediate value for the worker. -}
initialWorkerIntermediateValue ::
    VisitorMode visitor_mode →
    WorkerIntermediateValueFor visitor_mode
initialWorkerIntermediateValue AllMode = mempty
initialWorkerIntermediateValue FirstMode = ()
initialWorkerIntermediateValue (FoundModeUsingPull _) = mempty
initialWorkerIntermediateValue (FoundModeUsingPush _) = ()
