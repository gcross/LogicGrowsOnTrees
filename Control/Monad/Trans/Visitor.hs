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
-- @+node:gcross.20110722110408.1170: *3* gatherVisitorResults
gatherVisitorResults :: Visitor α → [α]
gatherVisitorResults = runIdentity . runVisitorTAndGatherResults
-- @+node:gcross.20110722110408.1171: *3* runAndCache
runAndCache :: Serialize x ⇒ m x → VisitorT m x
runAndCache = singleton . Cache
-- @+node:gcross.20110722110408.1167: *3* runVisitorTAndGatherResults
runVisitorTAndGatherResults :: Monad m ⇒ VisitorT m α → m [α]
runVisitorTAndGatherResults = viewT >=> \view →
    case view of
        Return x → return [x]
        (Cache mx :>>= k) → mx >>= runVisitorTAndGatherResults . k
        (Choice left right :>>= k) →
            liftM2 (++)
                (runVisitorTAndGatherResults $ left >>= k)
                (runVisitorTAndGatherResults $ right >>= k)
        (Null :>>= _) → return []
-- @+node:gcross.20110722110408.1182: *3* runVisitorT
runVisitorT :: Monad m ⇒ VisitorT m α → m ()
runVisitorT = viewT >=> \view →
    case view of
        Return x → return ()
        (Cache mx :>>= k) → mx >>= runVisitorT . k
        (Choice left right :>>= k) → do
            runVisitorT $ left >>= k
            runVisitorT $ right >>= k
        (Null :>>= _) → return ()
-- @-others
-- @-leo
