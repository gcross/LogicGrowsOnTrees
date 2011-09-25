-- @+leo-ver=5-thin
-- @+node:gcross.20110923120247.1201: * @file Control/Monad/Trans/Visitor/Path.hs
-- @@language haskell

-- @+<< Language extensions >>
-- @+node:gcross.20110923120247.1202: ** << Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- @-<< Language extensions >>

module Control.Monad.Trans.Visitor.Path where

-- @+<< Import needed modules >>
-- @+node:gcross.20110923120247.1203: ** << Import needed modules >>
import Control.Exception (Exception(),throw)
import Control.Monad ((>=>))
import Control.Monad.Operational (ProgramViewT(..),viewT)

import Data.ByteString (ByteString)
import Data.Functor.Identity (runIdentity)
import Data.Sequence (Seq,viewl,ViewL(..))
import Data.Serialize (Serialize(),decode)
import Data.Typeable (Typeable)

import Control.Monad.Trans.Visitor
-- @-<< Import needed modules >>

-- @+others
-- @+node:gcross.20110923120247.1204: ** Types
-- @+node:gcross.20110923120247.1206: *3* VisitorStep
data VisitorStep =
    CacheStep ByteString
 |  ChoiceStep Bool
-- @+node:gcross.20110923120247.1205: *3* VisitorPath
type VisitorPath = Seq VisitorStep
-- @+node:gcross.20110923120247.1209: *3* WalkError
data WalkError =
    DeadEnd
  | ChoiceStepAtCachePoint
  | CacheStepAtChoicePoint
  deriving (Show,Typeable)

instance Exception WalkError
-- @+node:gcross.20110923120247.1208: ** Functions
-- @+node:gcross.20110923164140.1190: *3* walkVisitor
walkVisitor :: VisitorPath → Visitor α → Visitor α
walkVisitor path = runIdentity . walkVisitorT path
-- @+node:gcross.20110923120247.1207: *3* walkVisitorT
walkVisitorT :: Monad m ⇒ VisitorPath → VisitorT m α → m (VisitorT m α)
walkVisitorT (viewl → EmptyL) = return
walkVisitorT path@(viewl → step :< tail) =
    viewT >=> \view → case (view,step) of
        (Return x,_) → throw DeadEnd
        (Null :>>= _,_) → throw DeadEnd
        (IsFirstVisit :>>= k,_) → walkVisitorT path $ k False
        (Cache _ :>>= k,CacheStep cache) → walkVisitorT tail $ either error k (decode cache)
        (Cache _ :>>= _,ChoiceStep _) → throw ChoiceStepAtCachePoint
        (Choice left _ :>>= k,ChoiceStep False) → walkVisitorT tail (left >>= k)
        (Choice _ right :>>= k,ChoiceStep True) → walkVisitorT tail (right >>= k)
        (Choice _ _ :>>= _,CacheStep _) → throw CacheStepAtChoicePoint
-- @-others
-- @-leo
