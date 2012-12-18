-- Language extensions {{{
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- }}}

module Control.Monad.Trans.Visitor where

-- Imports {{{
import Control.Applicative (Alternative(..),Applicative(..))
import Control.Monad (MonadPlus(..),(>=>),liftM,liftM2)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Operational (Program,ProgramT,ProgramViewT(..),singleton,view,viewT)
import Control.Monad.Trans.Class (MonadTrans(..))

import Data.Functor.Identity (Identity(..),runIdentity)
import Data.Monoid (Monoid(..))
import Data.Serialize (Serialize(),encode)
-- }}}

-- Classes {{{

class MonadPlus m ⇒ MonadVisitor m where -- {{{
    cache :: Serialize x ⇒ x → m x
    cacheGuard :: Bool → m ()
    cacheMaybe :: Serialize x ⇒ Maybe x → m x
-- }}}

class MonadPlus m ⇒ MonadVisitorTrans m where -- {{{
    type NestedMonadInVisitor m :: * → *
    runAndCache :: Serialize x ⇒ (NestedMonadInVisitor m) x → m x
    runAndCacheGuard :: (NestedMonadInVisitor m) Bool → m ()
    runAndCacheMaybe :: Serialize x ⇒ (NestedMonadInVisitor m) (Maybe x) → m x
-- }}}

-- }}}

-- Types {{{

data VisitorTInstruction m α where -- {{{
    Cache :: Serialize α ⇒ m (Maybe α) → VisitorTInstruction m α
    Choice :: VisitorT m α → VisitorT m α → VisitorTInstruction m α
    Null :: VisitorTInstruction m α
-- }}}
type VisitorInstruction = VisitorTInstruction Identity

newtype VisitorT m α = VisitorT { unwrapVisitorT :: ProgramT (VisitorTInstruction m) m α }
    deriving (Applicative,Functor,Monad,MonadIO)
type Visitor = VisitorT Identity
type VisitorIO = VisitorT IO

-- }}}

-- Instances {{{

instance Monad m ⇒ Alternative (VisitorT m) where -- {{{
    empty = mzero
    (<|>) = mplus
-- }}}

instance Eq α ⇒ Eq (Visitor α) where -- {{{
    (VisitorT x) == (VisitorT y) = e x y
      where
        e x y = case (view x, view y) of
            (Return x, Return y) → x == y
            (Null :>>= _, Null :>>= _) → True
            (Cache cx :>>= kx, Cache cy :>>= ky) →
                case (runIdentity cx, runIdentity cy) of
                    (Nothing, Nothing) → True
                    (Just x, Just y) → e (kx x) (ky y)
            (Choice (VisitorT ax) (VisitorT bx) :>>= kx, Choice (VisitorT ay) (VisitorT by) :>>= ky) →
                e (ax >>= kx) (ay >>= ky) && e (bx >>= kx) (by >>= ky)
-- }}}

instance Monad m ⇒ MonadPlus (VisitorT m) where -- {{{
    mzero = VisitorT . singleton $ Null
    left `mplus` right = VisitorT . singleton $ Choice left right
-- }}}

instance Monad m ⇒ MonadVisitor (VisitorT m) where -- {{{
    cache = runAndCache . return
    cacheGuard = runAndCacheGuard . return
    cacheMaybe = runAndCacheMaybe . return
-- }}}

instance Monad m ⇒ MonadVisitorTrans (VisitorT m) where -- {{{
    type NestedMonadInVisitor (VisitorT m) = m
    runAndCache = runAndCacheMaybe . liftM Just
    runAndCacheGuard = runAndCacheMaybe . liftM (\x → if x then Just () else Nothing)
    runAndCacheMaybe = VisitorT . singleton . Cache
-- }}}

instance MonadTrans VisitorT where -- {{{
    lift = VisitorT . lift
-- }}}

instance Monad m ⇒ Monoid (VisitorT m α) where -- {{{
    mempty = mzero
    mappend = mplus
    mconcat = msumBalanced
-- }}}

instance Show α ⇒ Show (Visitor α) where -- {{{
    show = s . unwrapVisitorT
      where
        s x = case view x of
            Return x → show x
            Null :>>= _ → "<NULL> >>= (...)"
            Cache c :>>= k →
                case runIdentity c of
                    Nothing → "NullCache"
                    Just x → "Cache[" ++ (show . encode $ x) ++ "] >>= " ++ (s (k x))
            Choice (VisitorT a) (VisitorT b) :>>= k → "(" ++ (s (a >>= k)) ++ ") | (" ++ (s (b >>= k)) ++ ")"
-- }}}

-- }}}

-- Functions {{{

allFrom :: MonadPlus m ⇒ [α] → m α -- {{{
allFrom = msumBalanced . map return
-- }}}

between :: (Enum n, MonadPlus m) ⇒ n → n → m n -- {{{
between x y =
    if a > b
        then mzero
        else go a b
  where
    a = fromEnum x
    b = fromEnum y

    go a b | a == b    = return (toEnum a)
    go a b | otherwise = go a (a+d) `mplus` go (a+d+1) b
      where
        d = (b-a) `div` 2
{-# INLINE between #-}
-- }}}

endowVisitor :: Monad m ⇒ Visitor α → VisitorT m α -- {{{
endowVisitor (view . unwrapVisitorT → Return x) = return x
endowVisitor (view . unwrapVisitorT → Cache mx :>>= k) =
    cacheMaybe (runIdentity mx) >>= endowVisitor . VisitorT . k
endowVisitor (view . unwrapVisitorT → Choice left right :>>= k) =
    mplus
        (endowVisitor left >>= endowVisitor . VisitorT . k)
        (endowVisitor right >>= endowVisitor . VisitorT . k)
endowVisitor (view . unwrapVisitorT → Null :>>= k) = mzero
-- }}}

msumBalanced :: MonadPlus m ⇒ [m α] → m α -- {{{
msumBalanced x = go (length x) x
  where
    go _ []  = mzero
    go _ [x] = x
    go n x   = go i a `mplus` go (n-i) b
      where
        (a,b) = splitAt i x
        i = n `div` 2
-- }}}

runVisitor :: Monoid α ⇒ Visitor α → α -- {{{
runVisitor = runIdentity . runVisitorT
-- }}}

runVisitorT :: (Monad m, Monoid α) ⇒ VisitorT m α → m α -- {{{
runVisitorT = viewT . unwrapVisitorT >=> \view →
    case view of
        Return x → return x
        (Cache mx :>>= k) → mx >>= maybe (return mempty) (runVisitorT . VisitorT . k)
        (Choice left right :>>= k) →
            liftM2 mappend
                (runVisitorT $ left >>= VisitorT . k)
                (runVisitorT $ right >>= VisitorT . k)
        (Null :>>= _) → return mempty
{-# SPECIALIZE runVisitorT :: Monoid α ⇒ Visitor α → Identity α #-}
{-# SPECIALIZE runVisitorT :: Monoid α ⇒ VisitorIO α → IO α #-}
-- }}}

runVisitorTAndIgnoreResults :: Monad m ⇒ VisitorT m α → m () -- {{{
runVisitorTAndIgnoreResults = viewT . unwrapVisitorT >=> \view →
    case view of
        Return x → return ()
        (Cache mx :>>= k) → mx >>= maybe (return ()) (runVisitorTAndIgnoreResults . VisitorT . k)
        (Choice left right :>>= k) → do
            runVisitorTAndIgnoreResults $ left >>= VisitorT . k
            runVisitorTAndIgnoreResults $ right >>= VisitorT . k
        (Null :>>= _) → return ()
{-# SPECIALIZE runVisitorTAndIgnoreResults :: Visitor α → Identity () #-}
{-# SPECIALIZE runVisitorTAndIgnoreResults :: VisitorIO α → IO () #-}
-- }}}

-- }}}
