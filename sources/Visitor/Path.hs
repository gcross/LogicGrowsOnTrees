-- Language extensions {{{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Visitor.Path where

-- Imports {{{
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
-- }}}

-- Exceptions {{{

data WalkError =
    VisitorTerminatedBeforeEndOfWalk
  | PastVisitorIsInconsistentWithPresentVisitor
  deriving (Eq,Show,Typeable)

instance Exception WalkError

-- }}}

-- Types {{{

data BranchChoice = -- {{{
    LeftBranch
  | RightBranch
  deriving (Eq,Ord,Read,Show)
$( derive makeSerialize ''BranchChoice )
-- }}}

data Step = -- {{{
    CacheStep ByteString
 |  ChoiceStep BranchChoice
 deriving (Eq,Ord,Show)
$( derive makeSerialize ''Step )
-- }}}

type Path = Seq Step

-- }}}

-- Functions {{{

oppositeBranchChoiceOf :: BranchChoice → BranchChoice -- {{{
oppositeBranchChoiceOf LeftBranch = RightBranch
oppositeBranchChoiceOf RightBranch = LeftBranch
-- }}}

sendTreeBuilderDownPath :: Path → TreeBuilder α → TreeBuilder α -- {{{
sendTreeBuilderDownPath path = runIdentity . sendTreeBuilderTDownPath path
-- }}}

sendTreeBuilderTDownPath :: Monad m ⇒ Path → TreeBuilderT m α → m (TreeBuilderT m α) -- {{{
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
-- }}}

-- }}}
