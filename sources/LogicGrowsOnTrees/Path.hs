{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

{-| This modules contains functionality relating to paths through trees. -}
module LogicGrowsOnTrees.Path
    (
    -- * Types
      BranchChoice(..)
    , Step(..)
    , Path
    -- * Functions
    , oppositeBranchChoiceOf
    , sendTreeDownPath
    , sendTreeTDownPath
    -- * Exceptions
    , WalkError(..)
    ) where

import Control.Exception (Exception(),throw)
import Control.Monad.Operational (ProgramViewT(..),viewT)

import Data.ByteString (ByteString)
import Data.Derive.Serialize
import Data.DeriveTH
import Data.Functor.Identity (runIdentity)
import Data.Sequence (Seq,viewl,ViewL(..))
import Data.Serialize
import Data.Typeable (Typeable)

import LogicGrowsOnTrees

--------------------------------------------------------------------------------
---------------------------------- Exceptions ----------------------------------
--------------------------------------------------------------------------------

{-| This exception is thrown whenever a 'Tree' is sent down a path which
    is incompatible with it.
 -}
data WalkError =
    {-| Indicates that a path is too long for a given tree --- i.e., the
        walk has hit a leaf (or a null) before the path finishes.
     -}
    TreeEndedBeforeEndOfWalk
    {-| Indicates that a choice step in a path coincided with a cache point in
        a tree, or vice versa.
     -}
  | PastTreeIsInconsistentWithPresentTree
  deriving (Eq,Show,Typeable)

instance Exception WalkError

--------------------------------------------------------------------------------
------------------------------------- Types ------------------------------------
--------------------------------------------------------------------------------

{-| 'Branch' represents a choice at a branch point to take either the left
    branch or the right branch.
 -}
data BranchChoice =
    LeftBranch
  | RightBranch
  deriving (Eq,Ord,Read,Show)
$( derive makeSerialize ''BranchChoice )

{-| 'Step' represents a step in a path through a tree, which can either pass
    through a point with a cached result or take a choice to go left or right
    at a branch point.
 -}
data Step =
    CacheStep ByteString {-^ Step through a cache point -}
 |  ChoiceStep BranchChoice {-^ Step through a choice point -}
 deriving (Eq,Ord,Show)
$( derive makeSerialize ''Step )

{-| A 'Path' is just a sequence of 'Step's. -}
type Path = Seq Step

--------------------------------------------------------------------------------
---------------------------------- Functions -----------------------------------
--------------------------------------------------------------------------------

{-| Returns the opposite of the given branch choice. -}
oppositeBranchChoiceOf :: BranchChoice → BranchChoice
oppositeBranchChoiceOf LeftBranch = RightBranch
oppositeBranchChoiceOf RightBranch = LeftBranch

{-| Has a 'Tree' follow a 'Path' guiding it to a particular subtree;  the
    main use case of this function is for a processor which has been given a
    particular subtree as its workload to get the tree to zoom in on
    that subtree.

    The way this function works is as follows: as long as the remaining path is
    non-empty, it runs the 'Tree' until it encounters either a cache
    point or a choice point; in the former case the path supplies the cached
    value in the 'CacheStep' constructor, and in the latter case the path
    supplies the branch to take in the 'ChoiceStep' constructor; when the
    remaining path is empty then the resulting 'Tree' is returned.

    WARNING: This function is /not/ valid for all inputs; it makes the
    assumption that the given 'Path' has been derived from the given
    'Tree' so that the path will always encounted choice points exactly
    when the tree does and likewise for cache points. Furthermore, the
    path must not run out before the tree hits a leaf. If any of these
    conditions is violated, a 'WalkError' exception will be thrown; in fact, you
    should hope than exception is thrown because it will let you know that there
    is a bug your code as the alternative is that you accidently give it a path
    that is not derived from the given tree but which coincidentally
    matches it which means that it will silently return a nonsense result.

    Having said all that, you should almost never need to worry about this
    possibility in practice because there is usually only one tree in
    use at a time and all paths in use have come from that tree.
 -}
sendTreeDownPath :: Path → Tree α → Tree α
sendTreeDownPath path = runIdentity . sendTreeTDownPath path

{-| See 'sendTreeDownPath';  the only difference is that this function
    works for impure trees.
 -}
sendTreeTDownPath :: Monad m ⇒ Path → TreeT m α → m (TreeT m α)
sendTreeTDownPath path tree =
    case viewl path of
        EmptyL → return tree
        step :< tail → do
            view ← viewT . unwrapTreeT $ tree
            case (view,step) of
                (Return _,_) →
                    throw TreeEndedBeforeEndOfWalk
                (Null :>>= _,_) →
                    throw TreeEndedBeforeEndOfWalk
                (Cache _ :>>= k,CacheStep cache) →
                    sendTreeTDownPath tail $ either error (TreeT . k) (decode cache)
                (Choice left _ :>>= k,ChoiceStep LeftBranch) →
                    sendTreeTDownPath tail (left >>= TreeT . k)
                (Choice _ right :>>= k,ChoiceStep RightBranch) →
                    sendTreeTDownPath tail (right >>= TreeT . k)
                (ProcessPendingRequests :>>= k,_) →
                    sendTreeTDownPath path (TreeT . k $ ())
                _ → throw PastTreeIsInconsistentWithPresentTree
