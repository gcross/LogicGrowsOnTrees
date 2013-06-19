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
    -- * Visitor functions
    -- $visitor
    , visitTreeWithinWorkload
    , visitTreeTWithinWorkload
    , visitTreeUntilFirstWithinWorkload
    , visitTreeTUntilFirstWithinWorkload
    , visitTreeUntilFoundWithinWorkload
    , visitTreeTUntilFoundWithinWorkload
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
----------------------------- Visitor functions --------------------------------
--------------------------------------------------------------------------------

{- $visitor
The functions in this section visit the part of a tree that is given by a
'Workload'.
-}

{-| Visits the nodes in a purely generated tree given by a 'Workload', and sums
    over and sums over all the results in the leaves.
 -}
visitTreeWithinWorkload ::
    Monoid α ⇒
    Workload →
    TreeGenerator α →
    α
visitTreeWithinWorkload Workload{..} =
    visitTreeStartingFromCheckpoint workloadCheckpoint
    .
    sendTreeGeneratorDownPath workloadPath

{-| Same as 'visitTreeWithinWorkload' but for an impurely generated tree. -}
visitTreeTWithinWorkload ::
    (Monad m, Monoid α) ⇒
    Workload →
    TreeGeneratorT m α →
    m α
visitTreeTWithinWorkload Workload{..} =
    sendTreeGeneratorTDownPath workloadPath
    >=>
    visitTreeTStartingFromCheckpoint workloadCheckpoint

{-| Visits the nodes in a purely generated tree given by a 'Workload' until
    a result (i.e. a leaf) has been found; if a result has been found then it is
    returned wrapped in 'Just', otherwise 'Nothing' is returned.
 -}
   
visitTreeUntilFirstWithinWorkload ::
    Workload →
    TreeGenerator α →
    Maybe α
visitTreeUntilFirstWithinWorkload Workload{..} =
    visitTreeUntilFirstStartingFromCheckpoint workloadCheckpoint
    .
    sendTreeGeneratorDownPath workloadPath

{-| Same as 'visitTreeUntilFirstWithinWorkload' but for an impurely generated tree. -}
visitTreeTUntilFirstWithinWorkload ::
    Monad m ⇒
    Workload →
    TreeGeneratorT m α →
    m (Maybe α)
visitTreeTUntilFirstWithinWorkload Workload{..} =
    sendTreeGeneratorTDownPath workloadPath
    >=>
    visitTreeTUntilFirstStartingFromCheckpoint workloadCheckpoint

{-| Visits the nodes in a purely generated tree given by a 'Workload', summing
    all results encountered (i.e., in the leaves) until the current partial sum
    satisfies the condition provided by the first function; if this condition is
    ever satisfied then its result is returned in 'Right', otherwise the final
    sum is returned in 'Left'.
 -}
visitTreeUntilFoundWithinWorkload ::
    Monoid α ⇒
    (α → Maybe β) →
    Workload →
    TreeGenerator α →
    Either α β
visitTreeUntilFoundWithinWorkload condition Workload{..} =
    visitTreeUntilFoundStartingFromCheckpoint condition workloadCheckpoint
    .
    sendTreeGeneratorDownPath workloadPath

{-| Same as 'visitTreeUntilFoundWithinWorkload' but for an impurely generated tree. -}
visitTreeTUntilFoundWithinWorkload ::
    (Monoid α, Monad m) ⇒
    (α → Maybe β) →
    Workload →
    TreeGeneratorT m α →
    m (Either α β)
visitTreeTUntilFoundWithinWorkload condition Workload{..} =
    sendTreeGeneratorTDownPath workloadPath
    >=>
    visitTreeTUntilFoundStartingFromCheckpoint condition workloadCheckpoint
