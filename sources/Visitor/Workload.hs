{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

{-| This module contains infrastructure for working with 'Workload's, which
    describe a portion of work to be performed by a worker.
 -}
module Visitor.Workload
    (
    -- * Workload type and simple functions
      Workload(..)
    , entire_workload
    , workloadDepth
    -- * Exploration functions
    -- $exploration
    , exploreTreeWithinWorkload
    , exploreTreeTWithinWorkload
    , exploreTreeUntilFirstWithinWorkload
    , exploreTreeTUntilFirstWithinWorkload
    , exploreTreeUntilFoundWithinWorkload
    , exploreTreeTUntilFoundWithinWorkload
    ) where

import Control.Monad ((>=>),join,liftM)
import Data.Derive.Serialize
import Data.DeriveTH
import Data.Function (on)
import Data.Monoid (Monoid(..))
import qualified Data.Sequence as Seq
import Data.Serialize

import Visitor
import Visitor.Checkpoint
import Visitor.Path

--------------------------------------------------------------------------------
-------------------- Workload type and simple functions ------------------------
--------------------------------------------------------------------------------

{-| A 'Workload' describes a portion of work to be performed by a worker;  it is
    nothing more than a 'Path' to the subtree where the workload is located
    paired with a 'Checkpoint' that indicates which parts of that subtree have
    already been explored.
 -}
data Workload = Workload
    {   workloadPath :: Path
    ,   workloadCheckpoint :: Checkpoint
    } deriving (Eq,Show)
$( derive makeSerialize ''Workload )

{-| Workloads are ordered first by their depth (the length of the 'Path'
    component), second by the value of the 'Path' component itself, and finally
    by the value of the 'Checkpoint' component. This ordering was chosen because
    there are places where it is nice to be able to conveniently order
    'Workload's by depth.
 -}
instance Ord Workload where
    x `compare` y =
        case (compare `on` workloadDepth) x y of
            EQ → case (compare `on` workloadPath) x y of
                EQ → (compare `on` workloadCheckpoint) x y
                c → c
            c → c

{-| This value represents a 'Workload' that consists of the entire tree. -}
entire_workload :: Workload
entire_workload = Workload Seq.empty Unexplored

{-| The depth of the workload, equal to the length of the 'Path' component. -}
workloadDepth :: Workload → Int
workloadDepth = Seq.length . workloadPath

--------------------------------------------------------------------------------
----------------------------- Exploration functions --------------------------------
--------------------------------------------------------------------------------

{- $exploration
The functions in this section explore the part of a tree that is given by a
'Workload'.
-}

{-| Explores the nodes in a pure tree given by a 'Workload', and sums
    over and sums over all the results in the leaves.
 -}
exploreTreeWithinWorkload ::
    Monoid α ⇒
    Workload →
    Tree α →
    α
exploreTreeWithinWorkload Workload{..} =
    exploreTreeStartingFromCheckpoint workloadCheckpoint
    .
    sendTreeDownPath workloadPath

{-| Same as 'exploreTreeWithinWorkload' but for an impure tree. -}
exploreTreeTWithinWorkload ::
    (Monad m, Monoid α) ⇒
    Workload →
    TreeT m α →
    m α
exploreTreeTWithinWorkload Workload{..} =
    sendTreeTDownPath workloadPath
    >=>
    exploreTreeTStartingFromCheckpoint workloadCheckpoint

{-| Explores the nodes in a pure tree given by a 'Workload' until
    a result (i.e. a leaf) has been found; if a result has been found then it is
    returned wrapped in 'Just', otherwise 'Nothing' is returned.
 -}
   
exploreTreeUntilFirstWithinWorkload ::
    Workload →
    Tree α →
    Maybe α
exploreTreeUntilFirstWithinWorkload Workload{..} =
    exploreTreeUntilFirstStartingFromCheckpoint workloadCheckpoint
    .
    sendTreeDownPath workloadPath

{-| Same as 'exploreTreeUntilFirstWithinWorkload' but for an impure tree. -}
exploreTreeTUntilFirstWithinWorkload ::
    Monad m ⇒
    Workload →
    TreeT m α →
    m (Maybe α)
exploreTreeTUntilFirstWithinWorkload Workload{..} =
    sendTreeTDownPath workloadPath
    >=>
    exploreTreeTUntilFirstStartingFromCheckpoint workloadCheckpoint

{-| Explores the nodes in a pure tree given by a 'Workload', summing
    all results encountered (i.e., in the leaves) until the current partial sum
    satisfies the condition provided by the first function; if this condition is
    ever satisfied then its result is returned in 'Right', otherwise the final
    sum is returned in 'Left'.
 -}
exploreTreeUntilFoundWithinWorkload ::
    Monoid α ⇒
    (α → Maybe β) →
    Workload →
    Tree α →
    Either α β
exploreTreeUntilFoundWithinWorkload condition Workload{..} =
    exploreTreeUntilFoundStartingFromCheckpoint condition workloadCheckpoint
    .
    sendTreeDownPath workloadPath

{-| Same as 'exploreTreeUntilFoundWithinWorkload' but for an impure tree. -}
exploreTreeTUntilFoundWithinWorkload ::
    (Monoid α, Monad m) ⇒
    (α → Maybe β) →
    Workload →
    TreeT m α →
    m (Either α β)
exploreTreeTUntilFoundWithinWorkload condition Workload{..} =
    sendTreeTDownPath workloadPath
    >=>
    exploreTreeTUntilFoundStartingFromCheckpoint condition workloadCheckpoint
