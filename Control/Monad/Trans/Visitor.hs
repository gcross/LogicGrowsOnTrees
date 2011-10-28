-- @+leo-ver=5-thin
-- @+node:gcross.20101114125204.1255: * @file Control/Monad/Trans/Visitor.hs
-- @@language haskell

-- @+<< Language extensions >>
-- @+node:gcross.20101114125204.1261: ** << Language extensions >>
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- @-<< Language extensions >>

module Control.Monad.Trans.Visitor where

-- @+<< Import needed modules >>
-- @+node:gcross.20101114125204.1256: ** << Import needed modules >>
import Control.Monad (MonadPlus(..),(>=>),liftM2)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Operational (Program,ProgramT,ProgramViewT(..),singleton,view,viewT)
import Control.Monad.Trans.Class (MonadTrans(..))

import Data.Functor.Identity (Identity(..),runIdentity)
import Data.Serialize (Serialize(),encode)
-- @-<< Import needed modules >>

-- @+others
-- @+node:gcross.20101114125204.1257: ** Types
-- @+node:gcross.20101114125204.1258: *3* VisitorTInstruction
data VisitorTInstruction m α where
    Cache :: Serialize α ⇒ m α → VisitorTInstruction m α
    Choice :: VisitorT m α → VisitorT m α → VisitorTInstruction m α
    Null :: VisitorTInstruction m α

type VisitorInstruction = VisitorTInstruction Identity
-- @+node:gcross.20110722110408.1165: *3* VisitorT
newtype VisitorT m α = VisitorT { unwrapVisitorT :: ProgramT (VisitorTInstruction m) m α }
-- @+node:gcross.20110722110408.1168: *3* Visitor
type Visitor = VisitorT Identity
-- @+node:gcross.20101114125204.1268: ** Instances
-- @+node:gcross.20111028170027.1311: *3* Eq Visitor
instance Eq α ⇒ Eq (Visitor α) where
    (VisitorT x) == (VisitorT y) = e x y
      where
        e x y = case (view x, view y) of
            (Return x, Return y) → x == y
            (Null :>>= _, Null :>>= _) → True
            (Cache cx :>>= kx, Cache cy :>>= ky) →
                (encode . runIdentity $ cx) == (encode . runIdentity $ cx) &&
                e (kx (runIdentity cx)) (ky (runIdentity cy))
            (Choice (VisitorT ax) (VisitorT bx) :>>= kx, Choice (VisitorT ay) (VisitorT by) :>>= ky) →
                e (ax >>= kx) (ay >>= ky) && e (bx >>= kx) (by >>= ky)
-- @+node:gcross.20110923164140.1205: *3* Monad VisitorT
instance Monad m ⇒ Monad (VisitorT m) where
    return = VisitorT . return
    mx >>= k = VisitorT $ (unwrapVisitorT mx) >>= (unwrapVisitorT . k)
-- @+node:gcross.20101114125204.1271: *3* MonadPlus VisitorT
instance Monad m ⇒ MonadPlus (VisitorT m) where
    mzero = VisitorT . singleton $ Null
    left `mplus` right = VisitorT . singleton $ Choice left right
-- @+node:gcross.20110923164140.1207: *3* MonadTrans VisitorT
instance MonadTrans VisitorT where
    lift = VisitorT . lift
-- @+node:gcross.20111028170027.1308: *3* Show Visitor
instance Show α ⇒ Show (Visitor α) where
    show = s . unwrapVisitorT
      where
        s x = case view x of
            Return x → show x
            Null :>>= _ → "<NULL> >>= (...)"
            Cache c :>>= k → "Cache[" ++ (show . encode . runIdentity $ c) ++ "] >>= " ++ (s (k (runIdentity c)))
            Choice (VisitorT a) (VisitorT b) :>>= k → "(" ++ (s (a >>= k)) ++ ") | (" ++ (s (b >>= k)) ++ ")"
-- @+node:gcross.20110722110408.1166: ** Functions
-- @+node:gcross.20110722110408.1180: *3* cache
cache :: (Monad m, Serialize x) ⇒ x → VisitorT m x
cache = VisitorT . singleton . Cache . return
-- @+node:gcross.20110722110408.1170: *3* gatherVisitorResults
gatherVisitorResults :: Visitor α → [α]
gatherVisitorResults = runIdentity . runVisitorTAndGatherResults
-- @+node:gcross.20110722110408.1171: *3* runAndCache
runAndCache :: Serialize x ⇒ m x → VisitorT m x
runAndCache = VisitorT . singleton . Cache
-- @+node:gcross.20110722110408.1182: *3* runVisitorT
runVisitorT :: Monad m ⇒ VisitorT m α → m ()
runVisitorT = viewT . unwrapVisitorT >=> \view →
    case view of
        Return x → return ()
        (Cache mx :>>= k) → mx >>= runVisitorT . VisitorT . k
        (Choice left right :>>= k) → do
            runVisitorT $ left >>= VisitorT . k
            runVisitorT $ right >>= VisitorT . k
        (Null :>>= _) → return ()
-- @+node:gcross.20110722110408.1167: *3* runVisitorTAndGatherResults
runVisitorTAndGatherResults :: Monad m ⇒ VisitorT m α → m [α]
runVisitorTAndGatherResults = viewT . unwrapVisitorT >=> \view →
    case view of
        Return x → return [x]
        (Cache mx :>>= k) → mx >>= runVisitorTAndGatherResults . VisitorT . k
        (Choice left right :>>= k) →
            liftM2 (++)
                (runVisitorTAndGatherResults $ left >>= VisitorT . k)
                (runVisitorTAndGatherResults $ right >>= VisitorT . k)
        (Null :>>= _) → return []
-- @-others
-- @-leo
