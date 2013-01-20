-- Language extensions {{{
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- }}}

module Control.Monad.Trans.Visitor -- {{{
    ( MonadVisitor(..)
    , MonadVisitorTrans(..)
    , Visitor
    , VisitorIO
    , VisitorT(..)
    , IntSum(..)
    , allFrom
    , allFromBalanced
    , allFromBalancedGreedy
    , between
    , endowVisitor
    , msumBalanced
    , msumBalancedGreedy
    , runVisitor
    , runVisitorT
    , runVisitorTAndIgnoreResults
    ) where -- }}}

-- Imports {{{
import Control.Applicative (Alternative(..),Applicative(..),liftA2)
import Control.Arrow ((***))
import Data.List (foldl1)
import Control.Monad (MonadPlus(..),(>=>),liftM,liftM2,msum)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))

import Data.Functor ((<$>))
import Data.Functor.Identity (Identity(..),runIdentity)
import Data.Monoid (Monoid(..),Sum())
import Data.Serialize (Serialize(),encode)

import Control.Monad.Trans.Visitor.Utils.MonadStacks
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

data VisitorT m α where -- {{{
    Return :: m α → VisitorT m α
    Deferred :: m β → (β → VisitorT m α) → VisitorT m α
    Cache :: Serialize β ⇒ m (Maybe β) → (β → VisitorT m α) → VisitorT m α
    Choice :: VisitorT m β → VisitorT m β → (β → VisitorT m α) → VisitorT m α
    Null :: VisitorT m α
-- }}}
type Visitor = VisitorT Identity
type VisitorIO = VisitorT IO

data IntSum = IntSum { getIntSum :: {-# UNPACK #-} !Int } deriving (Eq,Ord,Read,Show)
-- }}}

-- Instances {{{

instance Monoid IntSum where -- {{{
    mempty = IntSum 0
    {-# INLINE mempty #-}
    mappend !(IntSum x) !(IntSum y) = IntSum (x+y)
    {-# INLINE mappend #-}
    mconcat = go 0
      where
        go !accum [] = IntSum accum
        go !accum (IntSum x:xs) = go (accum+x) xs
    {-# INLINE mconcat #-}
-- }}}

instance Applicative m ⇒ Alternative (VisitorT m) where -- {{{
    empty = Null
    {-# INLINE empty #-}
    x <|> y = Choice x y pure
    {-# INLINE (<|>) #-}
-- }}}

instance Applicative m ⇒ Applicative (VisitorT m) where -- {{{
    pure = Return . pure
    {-# INLINE pure #-}

    Null <*> _ = Null
    _ <*> Null = Null
    Return mx1 <*> Return mx2 = Return (mx1 <*> mx2)
    Return mx <*> next = Deferred mx (<$> next)
    Deferred m1 k <*> next = Deferred m1 ((<*> next) . k)
    Cache m1 k <*> next = Cache m1 ((<*> next) . k)
    Choice l r k <*> next = Choice l r ((<*> next) . k)
    {-# INLINE (<*>) #-}
-- }}}

instance Eq α ⇒ Eq (Visitor α) where -- {{{
    Return x == Return y = runIdentity x == runIdentity y
    Deferred mx k == other = k (runIdentity mx) == other
    other == Deferred mx k = other == k (runIdentity mx)
    Null == Null = True
    Cache cx kx == Cache cy ky =
        case (runIdentity cx, runIdentity cy) of
            (Nothing, Nothing) → True
            (Just x, Just y) → kx x == ky y
    Choice ax bx kx == Choice ay by ky =
        (ax >>= kx) == (ay >>= ky) && (bx >>= kx) == (by >>= ky)
    _ == _ = False
-- }}}

instance Functor m ⇒ Functor (VisitorT m) where -- {{{
    fmap f (Return m) = Return (fmap f m)
    fmap f (Deferred m k) = Deferred m (fmap f . k)
    fmap f (Cache m k) = Cache m (fmap f . k)
    fmap f (Choice l r k) = Choice l r (fmap f . k)
    fmap _ Null = Null
-- }}}

instance Monad m ⇒ Monad (VisitorT m) where -- {{{
    return = Return . return
    {-# INLINE return #-}
    Return mx >>= k = Deferred mx k
    Deferred mx k >>= f = Deferred mx (k >=> f)
    Cache mx k >>= f = Cache mx (k >=> f)
    Choice l r k >>= f = Choice l r (k >=> f)
    Null >>= _ = Null
    {-# INLINE (>>=) #-}
-- }}}

instance MonadIO m ⇒ MonadIO (VisitorT m) where -- {{{
    liftIO = Return . liftIO
-- }}}

instance Monad m ⇒ MonadPlus (VisitorT m) where -- {{{
    mzero = Null
    {-# INLINE mzero #-}
    left `mplus` right = Choice left right return
    {-# INLINE mplus #-}
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
    runAndCacheMaybe = flip Cache return
-- }}}

instance MonadTrans VisitorT where -- {{{
    lift = Return
-- }}}

instance Monad m ⇒ Monoid (VisitorT m α) where -- {{{
    mempty = mzero
    {-# INLINE mempty #-}
    mappend = mplus
    {-# INLINE mappend #-}
    mconcat = msumBalanced
    {-# INLINE mconcat #-}
-- }}}

instance Show α ⇒ Show (Visitor α) where -- {{{
    show (Return x) = show (runIdentity x)
    show (Deferred x k) = show (k . runIdentity $ x)
    show Null = "<NULL>"
    show (Cache c k) =
        case runIdentity c of
            Nothing → "NullCache"
            Just x → "Cache[" ++ show (encode x) ++ "] >>= " ++ show (k x)
    show (Choice a b k) = "(" ++ show (a >>= k) ++ ") | (" ++ show (b >>= k) ++ ")"
-- }}}

-- }}}

-- Functions {{{

allFrom :: MonadPlus m ⇒ [α] → m α -- {{{
allFrom = msum . map return
{-# INLINE allFrom #-}
-- }}}

allFromBalanced :: MonadPlus m ⇒ [α] → m α -- {{{
allFromBalanced = msumBalanced . map return
{-# INLINE allFromBalanced #-}
-- }}}

allFromBalancedGreedy :: MonadPlus m ⇒ [α] → m α -- {{{
allFromBalancedGreedy = msumBalancedGreedy . map return
{-# INLINE allFromBalancedGreedy #-}
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
endowVisitor (Return mx) = Return (return $ runIdentity mx)
endowVisitor (Deferred mx k) = Deferred (return $ runIdentity mx) (endowVisitor . k)
endowVisitor (Cache mx k) = Cache (return $ runIdentity mx) (endowVisitor . k)
endowVisitor (Choice left right k) = Choice (endowVisitor left) (endowVisitor right) (endowVisitor . k)
endowVisitor Null = Null
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
{-# INLINE msumBalanced #-}
-- }}}

msumBalancedGreedy :: MonadPlus m ⇒ [m α] → m α -- {{{
msumBalancedGreedy = go emptyStacks
  where
    go !stacks [] = mergeStacks stacks
    go !stacks (x:xs) = go (addToStacks stacks x) xs
{-# INLINE msumBalancedGreedy #-}
-- }}}

runVisitor :: Monoid α ⇒ Visitor α → α -- {{{
runVisitor = go
  where
    go (Return x) = runIdentity x
    go (Deferred x k) = go (k (runIdentity x))
    go (Cache mx k) = maybe mempty (go . k) (runIdentity mx)
    go (Choice left right k) = go (left >>= k) `mappend` go (right >>= k)
    go Null = mempty
{-# INLINE runVisitor #-}
-- }}}

runVisitorT :: (Monad m, Monoid α) ⇒ VisitorT m α → m α -- {{{
runVisitorT = go
  where
    go (Return mx) = mx
    go (Deferred mx k) = mx >>= go . k
    go (Cache mx k) = mx >>= maybe (return mempty) (go . k)
    go (Choice left right k) = liftM2 mappend (go $ left >>= k) (go $ right >>= k)
    go Null = return mempty
{-# INLINE runVisitorT #-}
-- }}}

runVisitorTAndIgnoreResults :: Monad m ⇒ VisitorT m α → m () -- {{{
runVisitorTAndIgnoreResults = go
  where
    go (Return x) = return ()
    go (Deferred mx k) = mx >>= go . k
    go (Cache mx k) = mx >>= maybe (return ()) (go . k)
    go (Choice left right k) = go (left >>= k) >> go (right >>= k)
    go Null = return ()
{-# INLINE runVisitorTAndIgnoreResults #-}
-- }}}

-- }}}
