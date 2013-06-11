{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

{-| This modules contains functionality relating to paths through trees. -}
module Visitor.Path
    (
    -- * Types
      BranchChoice(..)
    , Step(..)
    , Path
    -- * Functions
    , oppositeBranchChoiceOf
    , sendTreeBuilderDownPath
    , sendTreeBuilderTDownPath
    -- * Exceptions
    , WalkError(..)
    ) where

import Control.Exception (Exception(),throw)
import Control.Monad ((>=>))
import Control.Monad.Operational (ProgramViewT(..),viewT)

import Data.ByteString (ByteString)
import Data.Derive.Serialize
import Data.DeriveTH
import Data.Functor.Identity (runIdentity)
import Data.Sequence (Seq,viewl,ViewL(..))
import Data.Serialize
import Data.Typeable (Typeable)

import Visitor

--------------------------------------------------------------------------------
---------------------------------- Exceptions ----------------------------------
--------------------------------------------------------------------------------

{-| This exception is thrown whenever a 'TreeBuilder' is sent down a path which
    is incompatible with it.
 -}
data WalkError =
    {-| Indicates that a path is too long for a given tree builder --- i.e., the
        builder hits a leaf before the path finishes.
     -}
    VisitorTerminatedBeforeEndOfWalk
    {-| Indicates that a choice step in a path coincided with a cache point in
        a tree builder, or vice versa.
     -}
  | PastVisitorIsInconsistentWithPresentVisitor
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

{-| Has a 'TreeBuilder' follow a 'Path' guiding it to a particular subtree;  the
    main use case of this function is for a processor which has been given a
    particular subtree as its workload to get the tree builder to zoom in on
    that subtree.

    The way this function works is as follows: as long as the remaining path is
    non-empty, it runs the 'TreeBuilder' until it encounters either a cache
    point or a choice point; in the former case the path supplies the cached
    value in the 'CacheStep' constructor, and in the latter case the path
    supplies the branch to take in the 'ChoiceStep' constructor; when the
    remaining path is empty then the resulting 'TreeBuilder' is returned.

    WARNING: This function is /not/ valid for all inputs; it makes the
    assumption that the given 'Path' has been derived from the given
    'TreeBuilder' so that the path will always encounted choice points exactly
    when the tree builder does and likewise for cache points. Furthermore, the
    path must not run out before the tree builder hits a leaf. If any of these
    conditions is violated, a 'WalkError' exception will be thrown; in fact, you
    should hope than exception is thrown because it will let you know that there
    is a bug your code as the alternative is that you accidently give it a path
    that is not derived from the given tree builder but which coincidentally
    matches it which means that it will silently return a nonsense result.

    Having said all that, you should almost never need to worry about this
    possibility in practice because there is usually only one tree builder in
    use at a time and all paths in use have come from that tree builder.
 -}
sendTreeBuilderDownPath :: Path → TreeBuilder α → TreeBuilder α
sendTreeBuilderDownPath path = runIdentity . sendTreeBuilderTDownPath path

{-| See 'sendTreeBuilderDownPath';  the only difference is that this function
    works for impure tree builders.
 -}
sendTreeBuilderTDownPath :: Monad m ⇒ Path → TreeBuilderT m α → m (TreeBuilderT m α)
sendTreeBuilderTDownPath path visitor =
    case viewl path of
        EmptyL → return visitor
        step :< tail → do
            view ← viewT . unwrapTreeBuilderT $ visitor
            case (view,step) of
                (Return _,_) →
                    throw VisitorTerminatedBeforeEndOfWalk
                (Null :>>= _,_) →
                    throw VisitorTerminatedBeforeEndOfWalk
                (Cache _ :>>= k,CacheStep cache) →
                    sendTreeBuilderTDownPath tail $ either error (TreeBuilderT . k) (decode cache)
                (Choice left _ :>>= k,ChoiceStep LeftBranch) →
                    sendTreeBuilderTDownPath tail (left >>= TreeBuilderT . k)
                (Choice _ right :>>= k,ChoiceStep RightBranch) →
                    sendTreeBuilderTDownPath tail (right >>= TreeBuilderT . k)
                _ → throw PastVisitorIsInconsistentWithPresentVisitor
