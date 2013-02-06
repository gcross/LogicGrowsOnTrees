-- Language extensions {{{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- }}}

module Control.Visitor.Path where

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

import Control.Visitor
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
$( derive makeSerialize ''Branch )
-- }}}

data VisitorStep = -- {{{
    CacheStep ByteString
 |  ChoiceStep Branch
 deriving (Eq,Ord,Show)
$( derive makeSerialize ''VisitorStep )
-- }}}

type VisitorPath = Seq VisitorStep

-- }}}

-- Functions {{{

oppositeBranchOf :: Branch → Branch -- {{{
oppositeBranchOf LeftBranch = RightBranch
oppositeBranchOf RightBranch = LeftBranch
-- }}}

sendVisitorDownPath :: VisitorPath → Visitor α → Visitor α -- {{{
sendVisitorDownPath path = runIdentity . sendVisitorTDownPath path
-- }}}

sendVisitorTDownPath :: Monad m ⇒ VisitorPath → VisitorT m α → m (VisitorT m α) -- {{{
sendVisitorTDownPath (viewl → EmptyL) = return
sendVisitorTDownPath path@(viewl → step :< tail) =
    viewT . unwrapVisitorT >=> \view → case (view,step) of
        (Return x,_) →
            throw VisitorTerminatedBeforeEndOfWalk
        (Null :>>= _,_) →
            throw VisitorTerminatedBeforeEndOfWalk
        (Cache _ :>>= k,CacheStep cache) →
            sendVisitorTDownPath tail $ either error (VisitorT . k) (decode cache)
        (Choice left _ :>>= k,ChoiceStep LeftBranch) →
            sendVisitorTDownPath tail (left >>= VisitorT . k)
        (Choice _ right :>>= k,ChoiceStep RightBranch) →
            sendVisitorTDownPath tail (right >>= VisitorT . k)
        _ → throw PastVisitorIsInconsistentWithPresentVisitor
-- }}}

-- }}}
