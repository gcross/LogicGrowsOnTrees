{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module Control.Visitor
    (
    -- * Visitor Features
    -- $type-classes
      MonadVisitor(..)
    , MonadVisitorTrans(..)
    -- * Visitor Types
    -- $types
    , Visitor
    , VisitorIO
    , VisitorT(..)
    -- * Functions
    -- $functions
    -- ** ...that run visitors
    -- $runners
    , runVisitor
    , runVisitorT
    , runVisitorTAndIgnoreResults
    , runVisitorUntilFirst
    , runVisitorTUntilFirst
    , runVisitorUntilFound
    , runVisitorTUntilFound
    -- ** ...that help creating visitors
    -- $helpers
    , allFrom
    , allFromBalanced
    , allFromBalancedGreedy
    , between
    , msumBalanced
    , msumBalancedGreedy
    -- ** ...that transform visitors
    , endowVisitor
    -- * Implementation
    , VisitorTInstruction(..)
    , VisitorInstruction
    ) where

import Control.Applicative (Alternative(..),Applicative(..))
import Control.Monad (MonadPlus(..),(>=>),guard,liftM,liftM2,msum)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Operational (ProgramT,ProgramViewT(..),singleton,view,viewT)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.List (ListT)
import Control.Monad.Trans.Maybe (MaybeT)

import Data.Functor.Identity (Identity(..),runIdentity)
import Data.Maybe (isJust)
import Data.Monoid ((<>),Monoid(..))
import Data.Serialize (Serialize(),encode)

import Control.Visitor.Utils.MonadStacks

--------------------------------------------------------------------------------
--------------------------------- Type-classes ---------------------------------
--------------------------------------------------------------------------------

{- $type-classes

Visitors are instances of 'MonadVisitor' and/or 'MonadVisitorTrans', which are
both subclasses of 'MonadPlus'.  The additional functionality offered by these
type-classes is the ability to cache results so that a computation does not need
to be repeated when a path through the visitor is taken a second time, which
happens either when resuming from a checkpoint or when a workload has been
stolen by another processor as the first step is to retrace the path through the
visitor that leads to the stolen workload.

These features could have been provided as functions, but there are two reasons
why they were subsumed into type-classes: first, because one might want to
add another layer above the 'Visitor' monad transformers in the monad stack
(as is the case in "Control.Visitor.Label"), and second, because one might want
to run a visitor using a simpler monad such as [] for testing purposes.

NOTE:  Caching a computation takes space in the 'Checkpoint', so it is something
       you should only do when the result is relatively small and the
       computation is very expensive and is high enough in the search tree that
       it is likely to be repeated often.  If the calculation is low enough in
       the search tree that it is unlikely to be repeated, is cheap enough so
       that repeating it is not a big deal, or produces a result with an
       incredibly large memory footprint, then you are probably better off not
       caching the result.
 -}


{-| The 'MonadVisitor' class provides caching functionality when running a pure
    visitor;  at minimum 'cacheMaybe' needs to be defined.
 -}
class MonadPlus m ⇒ MonadVisitor m where
    {-| Cache a value in case we visit this node again. -}
    cache :: Serialize x ⇒ x → m x
    cache = cacheMaybe . Just

    {-| This does the same thing as 'guard' but it caches the result. -}
    cacheGuard :: Bool → m ()
    cacheGuard = cacheMaybe . (\x → if x then Just () else Nothing)

    {-| This function is a combination of the previous two;  it performs a
        computation which might fail by returning 'Nothing', and if that happens
        it aborts the visitor;  if it passes then the result is cached and
        returned.

        Note that the previous two methods are essentially specializations of
        this method.
     -}
    cacheMaybe :: Serialize x ⇒ Maybe x → m x

{-| This class is like 'MonadVisitor', but it is designed to work with monad
    stacks;  at minimum 'runAndCacheMaybe' needs to be defined.
 -}
class (MonadPlus m, Monad (NestedMonadInVisitor m)) ⇒ MonadVisitorTrans m where
    {-| The next layer down in the monad transformer stack. -}
    type NestedMonadInVisitor m :: * → *

    {-| Runs the given action in the nested monad and caches the result. -}
    runAndCache :: Serialize x ⇒ (NestedMonadInVisitor m) x → m x
    runAndCache = runAndCacheMaybe . liftM Just

    {-| Runs the given action in the nested monad and then does the equivalent
        of feeding it into 'guard', caching the result.
     -}
    runAndCacheGuard :: (NestedMonadInVisitor m) Bool → m ()
    runAndCacheGuard = runAndCacheMaybe . liftM (\x → if x then Just () else Nothing)

    {-| Runs the given action in the nested monad;  if it returns 'Nothing',
        then it acts like 'mzero',  if it returns 'Just x', then it caches the
        result.
     -}
    runAndCacheMaybe :: Serialize x ⇒ (NestedMonadInVisitor m) (Maybe x) → m x

data VisitorTInstruction m α where
    Cache :: Serialize α ⇒ m (Maybe α) → VisitorTInstruction m α
    Choice :: VisitorT m α → VisitorT m α → VisitorTInstruction m α
    Null :: VisitorTInstruction m α

type VisitorInstruction = VisitorTInstruction Identity

{- $types
The following are the visitor types that are accepted by most of he functions in
this package.  You do not need to know the details of their definitions unless
you intend to write your own custom routines for running and transforming
visitors, in which case the relevant information is at the bottom of this page
in the Implementation` section.

There is one type of pure visitor and two types of impure visitors.  In general,
your visitor should nearly always be pure if you are planning to make use of
checkpointing or parallel runs, because in general parts of the visitor will be
run multiple times, some parts may not be run at all on a given processor, and
whenever a leaf is hit there will be a jump to a higher node, so if your visitor
is impure the effects need to be meaningful no matter how the visitor is run on
a given processor.

Having said that, there are a few times when an impure visitor can make sense:
first, if the inner monad is something like the `Reader` monad, which has no
side-effects;  second, for testing purposes (e.g., many of my tests of the
various visitor runners use `MVar`s and the like to ensure that visitors are
explored in a certain way to test certain code paths);  finally, if there is some
side-effectful action that you want to run on each result (such as storing a
result into a database), though in this case you will need to make sure that
your code is robust against being run multiple times as there is no guarantee in
an environment where the system might be shut down and resumed from a checkpoint
that your action will only have been run once on a given result (i.e., if the
system goes down after your action was run but before a checkpoint was made
marking that it was run).

If you need something like state in your visitor, then you should consider
nesting the visitor monad in the state monad rather than vice-versa, because
this will do things like automatically erasing the change in state that happened
between an inner node and a leaf when the visitor jumps back up from the leaf
to an inner node.
-}

{-| A pure visitor, which is what you should normally be using. -}
type Visitor = VisitorT Identity

{-| A visitor running in the I/O monad, which you should only be using for
    testing purposes or, say, if you are planning on storing each result in an
    external database, in which case you need to guard against the possibility
    that an action for a given result might be run twice in checkpointing and/or
    parallel settings.
-}
type VisitorIO = VisitorT IO


{-| A visitor run in an arbitrary monad. -}
newtype VisitorT m α = VisitorT { unwrapVisitorT :: ProgramT (VisitorTInstruction m) m α }
    deriving (Applicative,Functor,Monad,MonadIO)


--------------------------------------------------------------------------------
---------------------------------- Instances -----------------------------------
--------------------------------------------------------------------------------

{-| The 'Alternative' instance functions like the 'MonadPlus' instance. -}
instance Monad m ⇒ Alternative (VisitorT m) where
    empty = mzero
    (<|>) = mplus

{-| Two visitors are equal if the generate exactly the same tree. -}
instance Eq α ⇒ Eq (Visitor α) where
    (VisitorT x) == (VisitorT y) = e x y
      where
        e x y = case (view x, view y) of
            (Return x, Return y) → x == y
            (Null :>>= _, Null :>>= _) → True
            (Cache cx :>>= kx, Cache cy :>>= ky) →
                case (runIdentity cx, runIdentity cy) of
                    (Nothing, Nothing) → True
                    (Just x, Just y) → e (kx x) (ky y)
                    _ → False
            (Choice (VisitorT ax) (VisitorT bx) :>>= kx, Choice (VisitorT ay) (VisitorT by) :>>= ky) →
                e (ax >>= kx) (ay >>= ky) && e (bx >>= kx) (by >>= ky)
            _  → False

{-| For this type, 'mplus' provides a branch node with a choice between two
    values and 'mzero' provides a node with no values.
 -}
instance Monad m ⇒ MonadPlus (VisitorT m) where
    mzero = VisitorT . singleton $ Null
    left `mplus` right = VisitorT . singleton $ Choice left right

{-| This instance performs no caching but is provided to make it easier to test
    running a visitor using the List monad.
 -}
instance MonadVisitor [] where
    cacheMaybe = maybe mzero return

{-| This instance performs no caching but is provided to make it easier to test
    running a visitor using the 'ListT' monad.
 -}
instance Monad m ⇒ MonadVisitor (ListT m) where
    cacheMaybe = maybe mzero return

{-| Like the 'MonadVisitor' isntance, this instance does no caching. -}
instance Monad m ⇒ MonadVisitorTrans (ListT m) where
    type NestedMonadInVisitor (ListT m) = m
    runAndCacheMaybe = lift >=> maybe mzero return

{-| This instance performs no caching but is provided to make it easier to test
    running a visitor using the 'Maybe' monad.
 -}
instance MonadVisitor Maybe where
    cacheMaybe = maybe mzero return

{-| This instance performs no caching but is provided to make it easier to test
    running a visitor using the 'MaybeT' monad.
 -}
instance Monad m ⇒ MonadVisitor (MaybeT m) where
    cacheMaybe = maybe mzero return


{-| Like the 'MonadVisitor' isntance, this instance does no caching. -}
instance Monad m ⇒ MonadVisitorTrans (MaybeT m) where
    type NestedMonadInVisitor (MaybeT m) = m
    runAndCacheMaybe = lift >=> maybe mzero return

instance Monad m ⇒ MonadVisitor (VisitorT m) where
    cache = runAndCache . return
    cacheGuard = runAndCacheGuard . return
    cacheMaybe = runAndCacheMaybe . return

instance Monad m ⇒ MonadVisitorTrans (VisitorT m) where
    type NestedMonadInVisitor (VisitorT m) = m
    runAndCache = runAndCacheMaybe . liftM Just
    runAndCacheGuard = runAndCacheMaybe . liftM (\x → if x then Just () else Nothing)
    runAndCacheMaybe = VisitorT . singleton . Cache

{-| This instance allows you to automatically get a MonadVisitor instance for
    any monad transformer that has `MonadPlus` defined.  (Unfortunately its
    presence requires OverlappingInstances because it overlaps with the instance
    for `VisitorT`, even though the constraints are such that it is impossible
    in practice for there to ever be a case where a given type is satisfied by
    both instances.)
 -}
instance (MonadTrans t, MonadVisitor m, MonadPlus (t m)) ⇒ MonadVisitor (t m) where
    cache = lift . cache
    cacheGuard = lift . cacheGuard
    cacheMaybe = lift . cacheMaybe

instance MonadTrans VisitorT where
    lift = VisitorT . lift

{-| The 'Monoid' instance acts like the 'MonadPlus' instance. -}
instance Monad m ⇒ Monoid (VisitorT m α) where
    mempty = mzero
    mappend = mplus
    mconcat = msum

instance Show α ⇒ Show (Visitor α) where
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


--------------------------------------------------------------------------------
---------------------------------- Functions -----------------------------------
--------------------------------------------------------------------------------

{- $functions
There are three kinds of functions in this module:  functions which run visitors
in various ways, functions to make it easier to build visitors, and functions
which change the base monad of a pure visitor.
 -}

allFrom :: MonadPlus m ⇒ [α] → m α
allFrom = msum . map return
{-# INLINE allFrom #-}


allFromBalanced :: MonadPlus m ⇒ [α] → m α
allFromBalanced = msumBalanced . map return
{-# INLINE allFromBalanced #-}


allFromBalancedGreedy :: MonadPlus m ⇒ [α] → m α
allFromBalancedGreedy = msumBalancedGreedy . map return
{-# INLINE allFromBalancedGreedy #-}


between :: (Enum n, MonadPlus m) ⇒ n → n → m n
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


endowVisitor :: Monad m ⇒ Visitor α → VisitorT m α
endowVisitor visitor =
    case view . unwrapVisitorT $ visitor of
        Return x → return x
        Cache mx :>>= k →
            cacheMaybe (runIdentity mx) >>= endowVisitor . VisitorT . k
        Choice left right :>>= k →
            mplus
                (endowVisitor left >>= endowVisitor . VisitorT . k)
                (endowVisitor right >>= endowVisitor . VisitorT . k)
        Null :>>= _ → mzero


msumBalanced :: MonadPlus m ⇒ [m α] → m α
msumBalanced x = go (length x) x
  where
    go _ []  = mzero
    go _ [x] = x
    go n x   = go i a `mplus` go (n-i) b
      where
        (a,b) = splitAt i x
        i = n `div` 2
{-# INLINE msumBalanced #-}


msumBalancedGreedy :: MonadPlus m ⇒ [m α] → m α
msumBalancedGreedy = go emptyStacks
  where
    go !stacks [] = mergeStacks stacks
    go !stacks (x:xs) = go (addToStacks stacks x) xs
{-# INLINE msumBalancedGreedy #-}

{- $runners
The following functions all take a visitor as input and produce the resul of
running the visitor as output.  There are seven functions because there are two
kinds of visitors -- pure and impure -- and three modes for running the visitor
-- sum all result, return the first result, and gather results until they
satisfy a condition and then return -- plus a seventh function that runs a
visitor only for its side-effects.
 -}

{-| Run a pure visitor until all results have been found and summed together. -}
runVisitor :: Monoid α ⇒ Visitor α → α
runVisitor v =
    case view (unwrapVisitorT v) of
        Return !x → x
        (Cache mx :>>= k) → maybe mempty (runVisitor . VisitorT . k) (runIdentity mx)
        (Choice left right :>>= k) →
            let !x = runVisitor $ left >>= VisitorT . k
                !y = runVisitor $ right >>= VisitorT . k
                !xy = mappend x y
            in xy
        (Null :>>= _) → mempty
{-# INLINEABLE runVisitor #-}

{-| Run an impure visitor until all results have been found and summed together. -}
runVisitorT :: (Monad m, Monoid α) ⇒ VisitorT m α → m α
runVisitorT = viewT . unwrapVisitorT >=> \view →
    case view of
        Return !x → return x
        (Cache mx :>>= k) → mx >>= maybe (return mempty) (runVisitorT . VisitorT . k)
        (Choice left right :>>= k) →
            liftM2 (\(!x) (!y) → let !xy = mappend x y in xy)
                (runVisitorT $ left >>= VisitorT . k)
                (runVisitorT $ right >>= VisitorT . k)
        (Null :>>= _) → return mempty
{-# SPECIALIZE runVisitorT :: Monoid α ⇒ Visitor α → Identity α #-}
{-# SPECIALIZE runVisitorT :: Monoid α ⇒ VisitorIO α → IO α #-}
{-# INLINEABLE runVisitorT #-}

{-| Run an impure visitor for its side-effects, ignoring all results. -}
runVisitorTAndIgnoreResults :: Monad m ⇒ VisitorT m α → m ()
runVisitorTAndIgnoreResults = viewT . unwrapVisitorT >=> \view →
    case view of
        Return _ → return ()
        (Cache mx :>>= k) → mx >>= maybe (return ()) (runVisitorTAndIgnoreResults . VisitorT . k)
        (Choice left right :>>= k) → do
            runVisitorTAndIgnoreResults $ left >>= VisitorT . k
            runVisitorTAndIgnoreResults $ right >>= VisitorT . k
        (Null :>>= _) → return ()
{-# SPECIALIZE runVisitorTAndIgnoreResults :: Visitor α → Identity () #-}
{-# SPECIALIZE runVisitorTAndIgnoreResults :: VisitorIO α → IO () #-}
{-# INLINEABLE runVisitorTAndIgnoreResults #-}

{-| Run a pure visitor until a result has been found;  if a result has been
    found then it is returned wrapped in 'Just', otherwise 'Nothing' is returned.
 -}
runVisitorUntilFirst :: Visitor α → Maybe α
runVisitorUntilFirst v =
    case view (unwrapVisitorT v) of
        Return x → Just x
        (Cache mx :>>= k) → maybe Nothing (runVisitorUntilFirst . VisitorT . k) (runIdentity mx)
        (Choice left right :>>= k) →
            let x = runVisitorUntilFirst $ left >>= VisitorT . k
                y = runVisitorUntilFirst $ right >>= VisitorT . k
            in if isJust x then x else y
        (Null :>>= _) → Nothing
{-# INLINEABLE runVisitorUntilFirst #-}

{-| Same as 'runVisitorUntilFirst', but taking an impure visitor instead of a pure visitor. -}
runVisitorTUntilFirst :: Monad m ⇒ VisitorT m α → m (Maybe α)
runVisitorTUntilFirst = viewT . unwrapVisitorT >=> \view →
    case view of
        Return !x → return (Just x)
        (Cache mx :>>= k) → mx >>= maybe (return Nothing) (runVisitorTUntilFirst . VisitorT . k)
        (Choice left right :>>= k) → do
            x ← runVisitorTUntilFirst $ left >>= VisitorT . k
            if isJust x
                then return x
                else runVisitorTUntilFirst $ right >>= VisitorT . k
        (Null :>>= _) → return Nothing
{-# SPECIALIZE runVisitorTUntilFirst :: Visitor α → Identity (Maybe α) #-}
{-# SPECIALIZE runVisitorTUntilFirst :: VisitorIO α → IO (Maybe α) #-}
{-# INLINEABLE runVisitorTUntilFirst #-}

{-| Run a pure visitor summing all results encountered until the current sum
    satisfies the condition provided by the function in the first argument;  if
    the sum never satisfies the condition function then it (that is, the sum
    over all results) is returned wrapped in 'Left', otherwise the transformed
    result returned by the condition function is returned wrapped in 'Right'.
 -}
runVisitorUntilFound :: Monoid α ⇒ (α → Maybe β) → Visitor α → Either α β
runVisitorUntilFound f v =
    case view (unwrapVisitorT v) of
        Return x → runThroughFilter x
        (Cache mx :>>= k) →
            maybe (Left mempty) (runVisitorUntilFound f . VisitorT . k)
            $
            runIdentity mx
        (Choice left right :>>= k) →
            let x = runVisitorUntilFound f $ left >>= VisitorT . k
                y = runVisitorUntilFound f $ right >>= VisitorT . k
            in case (x,y) of
                (result@(Right _),_) → result
                (_,result@(Right _)) → result
                (Left a,Left b) → runThroughFilter (a <> b)
        (Null :>>= _) → Left mempty
  where
    runThroughFilter x = maybe (Left x) Right . f $ x

{-| Same as 'runVisitorUntilFound', but taking an impure visitor instead of a pure visitor. -}
runVisitorTUntilFound :: (Monad m, Monoid α) ⇒ (α → Maybe β) → VisitorT m α → m (Either α β)
runVisitorTUntilFound f = viewT . unwrapVisitorT >=> \view →
    case view of
        Return x → runThroughFilter x
        (Cache mx :>>= k) →
            mx
            >>=
            maybe (return (Left mempty)) (runVisitorTUntilFound f . VisitorT . k)
        (Choice left right :>>= k) → do
            x ← runVisitorTUntilFound f $ left >>= VisitorT . k
            case x of
                result@(Right _) → return result
                Left a → do
                    y ← runVisitorTUntilFound f $ right >>= VisitorT . k
                    case y of
                        result@(Right _) → return result
                        Left b → runThroughFilter (a <> b)
        (Null :>>= _) → return (Left mempty)
  where
    runThroughFilter x = return . maybe (Left x) Right . f $ x
