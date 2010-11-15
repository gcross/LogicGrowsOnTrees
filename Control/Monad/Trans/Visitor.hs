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
import Data.Sequence (Seq,ViewL(EmptyL,(:<)),(|>),viewl)
-- @nonl
-- @-node:gcross.20101114125204.1256:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20101114125204.1257:Types
-- @+node:gcross.20101114125204.1293:Branch
type Branch = Seq Choice
-- @nonl
-- @-node:gcross.20101114125204.1293:Branch
-- @+node:gcross.20101114125204.1265:Choice
data Choice = L | R deriving (Eq,Ord,Enum,Read,Show)
-- @nonl
-- @-node:gcross.20101114125204.1265:Choice
-- @+node:gcross.20101114125204.1258:VisitorT
newtype VisitorT v m α = VisitorT { runVisitorT :: (v → Choice → Maybe v) → v → m (v,α) }
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
label :: VisitorT Branch m α → m (Branch,α)
label v = runVisitorT v (\v b → Just (v |> b)) Seq.empty
-- @nonl
-- @-node:gcross.20101114125204.1274:label
-- @+node:gcross.20101114125204.1276:visit
visit :: Monad m ⇒ m α → VisitorT v m α
visit mx = VisitorT $ \_ v → mx >>= return . ((,) v)
-- @nonl
-- @-node:gcross.20101114125204.1276:visit
-- @+node:gcross.20101114125204.1295:walkDownBranch
walkDownBranch :: Functor m ⇒ Branch → VisitorT Branch m α → m α
walkDownBranch branch v = fmap snd (runVisitorT v descend branch)
  where
    descend branch choice =
        case viewl branch of
            EmptyL → Just branch
            head :< tail | choice == head → Just tail
            _ → Nothing
-- @nonl
-- @-node:gcross.20101114125204.1295:walkDownBranch
-- @-node:gcross.20101114125204.1273:Functions
-- @-others
-- @nonl
-- @-node:gcross.20101114125204.1255:@thin Control/Monad/Trans/Visitor.hs
-- @-leo
