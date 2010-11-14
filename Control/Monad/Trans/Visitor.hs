-- @+leo-ver=4-thin
-- @+node:gcross.20101114125204.1255:@thin Control/Monad/Trans/Visitor.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20101114125204.1261:<< Language extensions >>
{-# LANGUAGE UnicodeSyntax #-}
-- @nonl
-- @-node:gcross.20101114125204.1261:<< Language extensions >>
-- @nl

module Control.Monad.Trans.Visitor where

-- @<< Import needed modules >>
-- @+node:gcross.20101114125204.1256:<< Import needed modules >>
import Control.Arrow (second)
import Control.Monad (MonadPlus(..))

import qualified Data.Sequence as Seq
import Data.Sequence (Seq,(|>))
-- @nonl
-- @-node:gcross.20101114125204.1256:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20101114125204.1257:Types
-- @+node:gcross.20101114125204.1265:Branch
data Branch = L | R deriving (Eq,Ord,Enum,Read,Show)
-- @nonl
-- @-node:gcross.20101114125204.1265:Branch
-- @+node:gcross.20101114125204.1258:VisitorT
newtype VisitorT v m α = VisitorT { runVisitorT :: (v → Branch → Maybe v) → v → m (v,α) }
-- @nonl
-- @-node:gcross.20101114125204.1258:VisitorT
-- @-node:gcross.20101114125204.1257:Types
-- @+node:gcross.20101114125204.1268:Instances
-- @+node:gcross.20101114125204.1270:Functor VisitorT
instance Functor m ⇒ Functor (VisitorT v m) where
    fmap f mx = VisitorT $ \descend v → fmap (second f) (runVisitorT mx descend v)
-- @nonl
-- @-node:gcross.20101114125204.1270:Functor VisitorT
-- @+node:gcross.20101114125204.1269:Monad VisitorT
instance Monad m ⇒ Monad (VisitorT v m) where
    return x = VisitorT $ \_ v → return (v,x)
    mx >>= f =
        VisitorT $ \descend v → do
            (v,x) ← runVisitorT mx descend v
            runVisitorT (f x) descend v
    mx >> my =
        VisitorT $ \descend v → do
            (v,_) ← runVisitorT mx descend v
            runVisitorT my descend v
-- @nonl
-- @-node:gcross.20101114125204.1269:Monad VisitorT
-- @+node:gcross.20101114125204.1271:MonadPlus VisitorT
instance MonadPlus m ⇒ MonadPlus (VisitorT v m) where
    mzero = VisitorT $ \_ _ → mzero
    mx `mplus` my = VisitorT $ \descend v →
        case (descend v L,descend v R) of
            (Just v1,Just v2) →
                (runVisitorT mx descend v1)
                `mplus`
                (runVisitorT my descend v2)
            (Just v,Nothing) → runVisitorT mx descend v
            (Nothing, Just v) → runVisitorT my descend v
            (Nothing,Nothing) → mzero
-- @nonl
-- @-node:gcross.20101114125204.1271:MonadPlus VisitorT
-- @-node:gcross.20101114125204.1268:Instances
-- @+node:gcross.20101114125204.1273:Functions
-- @+node:gcross.20101114125204.1274:label
label :: VisitorT (Seq Branch) m a → m (Seq Branch,a)
label v = runVisitorT v (\v b → Just (v |> b)) Seq.empty
-- @nonl
-- @-node:gcross.20101114125204.1274:label
-- @+node:gcross.20101114125204.1276:visit
visit :: Monad m ⇒ m a → VisitorT v m a
visit mx = VisitorT $ \_ v → mx >>= return . ((,) v)
-- @nonl
-- @-node:gcross.20101114125204.1276:visit
-- @-node:gcross.20101114125204.1273:Functions
-- @-others
-- @nonl
-- @-node:gcross.20101114125204.1255:@thin Control/Monad/Trans/Visitor.hs
-- @-leo
