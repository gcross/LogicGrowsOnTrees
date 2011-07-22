-- @+leo-ver=5-thin
-- @+node:gcross.20101114125204.1255: * @file Control/Monad/Trans/Visitor.hs
-- @@language haskell

-- @+<< Language extensions >>
-- @+node:gcross.20101114125204.1261: ** << Language extensions >>
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-<< Language extensions >>

module Control.Monad.Trans.Visitor where

-- @+<< Import needed modules >>
-- @+node:gcross.20101114125204.1256: ** << Import needed modules >>
import Control.Monad (MonadPlus(..),(>=>),liftM2)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Operational (ProgramT,ProgramViewT(..),singleton,viewT)
import Control.Monad.Trans.Class (MonadTrans)

import Data.Functor.Identity (Identity,runIdentity)
import Data.Serialize (Serialize())
-- @-<< Import needed modules >>

-- @+others
-- @+node:gcross.20101114125204.1257: ** Types
-- @+node:gcross.20101114125204.1258: *3* VisitorInstruction
data VisitorInstruction m α where
    Cache :: Serialize α ⇒ m α → VisitorInstruction m α
    Choice :: VisitorT m α → VisitorT m α → VisitorInstruction m α
    IsFirstVisit :: VisitorInstruction m Bool
    Null :: VisitorInstruction m α
-- @+node:gcross.20110722110408.1165: *3* VisitorT
type VisitorT m = ProgramT (VisitorInstruction m) m
-- @+node:gcross.20110722110408.1168: *3* Visitor
type Visitor = VisitorT Identity
-- @+node:gcross.20101114125204.1268: ** Instances
-- @+node:gcross.20101114125204.1271: *3* MonadPlus VisitorT
instance Monad m ⇒ MonadPlus (VisitorT m) where
    mzero = singleton Null
    left `mplus` right = singleton $ Choice left right
-- @+node:gcross.20110722110408.1166: ** Functions
-- @+node:gcross.20110722110408.1180: *3* cache
cache :: (Monad m, Serialize x) ⇒ x → VisitorT m x
cache = singleton . Cache . return
-- @+node:gcross.20110722110408.1170: *3* gatherResults
gatherVisitorResults :: Visitor α → [α]
gatherVisitorResults = runIdentity . runVisitorAndGatherResults
-- @+node:gcross.20110722110408.1172: *3* isFirstVisit
isFirstVisit :: VisitorT m Bool
isFirstVisit = singleton IsFirstVisit
-- @+node:gcross.20110722110408.1171: *3* runAndCache
runAndCache :: Serialize x ⇒ m x → VisitorT m x
runAndCache = singleton . Cache
-- @+node:gcross.20110722110408.1167: *3* runVisitorAndGatherResults
runVisitorAndGatherResults :: Monad m ⇒ VisitorT m α → m [α]
runVisitorAndGatherResults = viewT >=> \view →
    case view of
        Return x → return [x]
        (Cache mx :>>= k) → mx >>= runVisitorAndGatherResults . k
        (Choice left right :>>= k) →
            liftM2 (++)
                (runVisitorAndGatherResults $ left >>= k)
                (runVisitorAndGatherResults $ right >>= k)
        (IsFirstVisit :>>= k) → runVisitorAndGatherResults $ k True
        (Null :>>= _) → return []
-- @-others
-- @-leo
