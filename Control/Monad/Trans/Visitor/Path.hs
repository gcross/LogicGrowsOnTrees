-- Language extensions {{{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- }}}

module Control.Monad.Trans.Visitor.Path where

-- Imports {{{
import Control.Exception (Exception(),throw)
import Control.Monad ((>=>))
import Control.Monad.Operational (ProgramViewT(..),viewT)

import Data.ByteString (ByteString)
import Data.Functor.Identity (runIdentity)
import Data.Sequence (Seq,viewl,ViewL(..))
import Data.Serialize (Serialize(),decode)
import Data.Typeable (Typeable)

import Control.Monad.Trans.Visitor
-- }}}

-- Exceptions {{{

data VisitorWalkError =
    VisitorTerminatedBeforeEndOfWalk
  | PastVisitorIsInconsistentWithPresentVisitor
  deriving (Eq,Show,Typeable)

instance Exception VisitorWalkError

-- }}}

-- Types {{{

data Branch = -- {{{
    LeftBranch
  | RightBranch
  deriving (Eq,Ord,Read,Show)
-- }}}

data VisitorStep = -- {{{
    CacheStep ByteString
 |  ChoiceStep Branch
 deriving (Eq,Show)
-- }}}

type VisitorPath = Seq VisitorStep

-- }}}

-- Functions {{{

oppositeBranchOf :: Branch → Branch -- {{{
oppositeBranchOf LeftBranch = RightBranch
oppositeBranchOf RightBranch = LeftBranch
-- }}}

walkVisitorDownPath :: VisitorPath → Visitor α → Visitor α -- {{{
walkVisitorDownPath path = runIdentity . walkVisitorTDownPath path
-- }}}

walkVisitorTDownPath :: Monad m ⇒ VisitorPath → VisitorT m α → m (VisitorT m α) -- {{{
walkVisitorTDownPath (viewl → EmptyL) = return
walkVisitorTDownPath path@(viewl → step :< tail) =
    viewT . unwrapVisitorT >=> \view → case (view,step) of
        (Return x,_) →
            throw VisitorTerminatedBeforeEndOfWalk
        (Null :>>= _,_) →
            throw VisitorTerminatedBeforeEndOfWalk
        (Cache _ :>>= k,CacheStep cache) →
            walkVisitorTDownPath tail $ either error (VisitorT . k) (decode cache)
        (Choice left _ :>>= k,ChoiceStep LeftBranch) →
            walkVisitorTDownPath tail (left >>= VisitorT . k)
        (Choice _ right :>>= k,ChoiceStep RightBranch) →
            walkVisitorTDownPath tail (right >>= VisitorT . k)
        _ → throw PastVisitorIsInconsistentWithPresentVisitor
-- }}}

-- }}}
