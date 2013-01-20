-- Language extensions {{{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- }}}

module Control.Monad.Trans.Visitor.Path where

-- Imports {{{
import Control.Exception (Exception(),throw)
import Control.Monad ((>=>))

import Data.ByteString (ByteString)
import Data.Derive.Serialize
import Data.DeriveTH
import Data.Functor.Identity (runIdentity)
import Data.Sequence (Seq,viewl,ViewL(..))
import Data.Serialize
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
sendVisitorTDownPath = go
  where
    go (viewl → EmptyL) visitor = return visitor
    go _ (Return _) = throw VisitorTerminatedBeforeEndOfWalk
    go _ Null = throw VisitorTerminatedBeforeEndOfWalk
    go path (Deferred mx k) = mx >>= go path . k
    go (viewl → CacheStep cache :< tail) (Cache _ k) = either error (go tail . k) (decode cache)
    go (viewl → ChoiceStep LeftBranch :< tail) (Choice l _ k) = go tail (l >>= k)
    go (viewl → ChoiceStep RightBranch :< tail) (Choice _ r k) = go tail (r >>= k)
    go _ _ = throw PastVisitorIsInconsistentWithPresentVisitor
-- }}}

-- }}}
