-- Language extensions {{{
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- }}}

module Control.Visitor.Checkpoint where

-- Imports {{{
import Control.Exception (Exception(),throw)
import Control.Monad ((>=>),join,liftM)
import Control.Monad.Operational (ProgramViewT(..),viewT)
import Control.Monad.Trans.Class (MonadTrans(..))

import Data.ByteString (ByteString)
import Data.Composition
import Data.Derive.Monoid
import Data.Derive.Serialize
import Data.DeriveTH
import Data.Functor.Identity (Identity,runIdentity)
import Data.Maybe (mapMaybe)
import Data.Monoid (Monoid(..))
import Data.Sequence ((|>),Seq,viewl,ViewL(..),viewr,ViewR(..))
import qualified Data.Sequence as Seq
import Data.Serialize
import Data.Typeable (Typeable)

import Control.Visitor
import Control.Visitor.Path
-- }}}

-- Types {{{

data Checkpoint = -- {{{
    CacheCheckpoint ByteString Checkpoint
  | ChoiceCheckpoint Checkpoint Checkpoint
  | Explored
  | Unexplored
  deriving (Eq,Ord,Read,Show)
$( derive makeSerialize ''Checkpoint )
-- }}}

type CheckpointCursor = Seq CheckpointDifferential

data CheckpointDifferential = -- {{{
    CacheCheckpointD ByteString
  | ChoiceCheckpointD Branch Checkpoint
  deriving (Eq,Read,Show)
-- }}}

type Context m α = Seq (ContextStep m α)

data ContextStep m α = -- {{{
    CacheContextStep ByteString
  | LeftBranchContextStep Checkpoint (VisitorT m α)
  | RightBranchContextStep
-- }}}

type ContextUpdate m α = -- {{{
    Context m α →
    Maybe (Context m α, Checkpoint, VisitorT m α)
-- }}}

data Progress α = Progress -- {{{
    {   progressCheckpoint :: Checkpoint
    ,   progressResult :: α
    } deriving (Eq,Show)
$( derive makeMonoid ''Progress )
$( derive makeSerialize ''Progress )
-- }}}

newtype ResultFetcher m α = ResultFetcher -- {{{
    {   fetchResult :: m (Maybe (α, Checkpoint, ResultFetcher m α))
    }
-- }}}

data VisitorTState m α = VisitorTState -- {{{
    {   visitorStateContext :: !(Context m α)
    ,   visitorStateCheckpoint :: !Checkpoint
    ,   visitorStateVisitor :: !(VisitorT m α)
    }
-- }}}
type VisitorState = VisitorTState Identity

-- }}}

-- Exceptions {{{

data InconsistentCheckpoints = InconsistentCheckpoints Checkpoint Checkpoint deriving (Eq,Show,Typeable)

instance Exception InconsistentCheckpoints

-- }}}

-- Instances {{{

instance Monoid Checkpoint where -- {{{
    mempty = Unexplored
    Explored `mappend` _ = Explored
    _ `mappend` Explored = Explored
    Unexplored `mappend` x = x
    x `mappend` Unexplored = x
    (ChoiceCheckpoint lx rx) `mappend` (ChoiceCheckpoint ly ry) =
        mergeCheckpointRoot (ChoiceCheckpoint (lx `mappend` ly) (rx `mappend` ry))
    (CacheCheckpoint cx x) `mappend` (CacheCheckpoint cy y)
      | cx == cy = mergeCheckpointRoot (CacheCheckpoint cx (x `mappend` y))
    mappend x y = throw (InconsistentCheckpoints x y)
-- }}}

instance Show (ContextStep m α) where -- {{{
    show (CacheContextStep c) = "CacheContextStep[" ++ show c ++ "]"
    show (LeftBranchContextStep checkpoint _) = "LeftBranchContextStep(" ++ show checkpoint ++ ")"
    show RightBranchContextStep = "RightRightBranchContextStep"
-- }}}

-- }}}

-- Functions {{{

checkpointFromContext :: Context m α → Checkpoint → Checkpoint -- {{{
checkpointFromContext = checkpointFromSequence $
    \step → case step of
        CacheContextStep cache → CacheCheckpoint cache
        LeftBranchContextStep right_checkpoint _ → flip ChoiceCheckpoint right_checkpoint
        RightBranchContextStep → ChoiceCheckpoint Explored
-- }}}

checkpointFromCursor :: CheckpointCursor → Checkpoint → Checkpoint -- {{{
checkpointFromCursor = checkpointFromSequence $
    \step → case step of
        CacheCheckpointD cache → CacheCheckpoint cache
        ChoiceCheckpointD LeftBranch right_checkpoint → flip ChoiceCheckpoint right_checkpoint
        ChoiceCheckpointD RightBranch left_checkpoint → ChoiceCheckpoint left_checkpoint
-- }}}

checkpointFromInitialPath :: Path → Checkpoint → Checkpoint -- {{{
checkpointFromInitialPath = checkpointFromSequence $
    \step → case step of
        CacheStep c → CacheCheckpoint c
        ChoiceStep LeftBranch → flip ChoiceCheckpoint Unexplored
        ChoiceStep RightBranch → ChoiceCheckpoint Unexplored
-- }}}

checkpointFromSequence :: -- {{{
    (α → (Checkpoint → Checkpoint)) →
    Seq α →
    Checkpoint →
    Checkpoint
checkpointFromSequence processStep (viewr → EmptyR) = id
checkpointFromSequence processStep (viewr → rest :> step) =
    checkpointFromSequence processStep rest
    .
    mergeCheckpointRoot
    .
    processStep step
-- }}}

checkpointFromUnexploredPath :: Path → Checkpoint -- {{{
checkpointFromUnexploredPath path = checkpointFromSequence
    (\step → case step of
        CacheStep c → CacheCheckpoint c
        ChoiceStep LeftBranch → flip ChoiceCheckpoint Explored
        ChoiceStep RightBranch → ChoiceCheckpoint Explored
    )
    path
    Unexplored
-- }}}

gatherResults :: -- {{{
    (Monad m, Monoid α) ⇒
    ResultFetcher m α →
    m α
gatherResults = go mempty
  where
    go result =
        fetchResult
        >=>
        maybe
            (return result)
            (\(result,_,fetcher) → go result fetcher)
-- }}}

initialVisitorState :: Checkpoint → VisitorT m α → VisitorTState m α -- {{{
initialVisitorState = VisitorTState Seq.empty
-- }}}

invertCheckpoint :: Checkpoint → Checkpoint -- {{{
invertCheckpoint Explored = Unexplored
invertCheckpoint Unexplored = Explored
invertCheckpoint (CacheCheckpoint cache rest) =
    mergeCheckpointRoot (CacheCheckpoint cache (invertCheckpoint rest))
invertCheckpoint (ChoiceCheckpoint left right) =
    mergeCheckpointRoot (ChoiceCheckpoint (invertCheckpoint left) (invertCheckpoint right))
-- }}}

mergeAllCheckpointNodes :: Checkpoint → Checkpoint -- {{{
mergeAllCheckpointNodes (ChoiceCheckpoint left right) = mergeCheckpointRoot (ChoiceCheckpoint (mergeAllCheckpointNodes left) (mergeAllCheckpointNodes right))
mergeAllCheckpointNodes (CacheCheckpoint cache checkpoint) = mergeCheckpointRoot (CacheCheckpoint cache (mergeAllCheckpointNodes checkpoint))
mergeAllCheckpointNodes checkpoint = checkpoint
-- }}}

mergeCheckpointRoot :: Checkpoint → Checkpoint -- {{{
mergeCheckpointRoot (ChoiceCheckpoint Unexplored Unexplored) = Unexplored
mergeCheckpointRoot (ChoiceCheckpoint Explored Explored) = Explored
mergeCheckpointRoot (CacheCheckpoint _ Explored) = Explored
mergeCheckpointRoot checkpoint = checkpoint
-- }}}

pathFromContext :: Context m α → Path -- {{{
pathFromContext = fmap pathStepFromContextStep
-- }}}

pathFromCursor :: CheckpointCursor → Path -- {{{
pathFromCursor = fmap pathStepFromCursorDifferential
-- }}}

pathStepFromContextStep :: ContextStep m α → Step -- {{{
pathStepFromContextStep (CacheContextStep cache) = CacheStep cache
pathStepFromContextStep (LeftBranchContextStep _ _) = ChoiceStep LeftBranch
pathStepFromContextStep (RightBranchContextStep) = ChoiceStep RightBranch
-- }}}

pathStepFromCursorDifferential :: CheckpointDifferential → Step -- {{{
pathStepFromCursorDifferential (CacheCheckpointD cache) = CacheStep cache
pathStepFromCursorDifferential (ChoiceCheckpointD active_branch _) = ChoiceStep active_branch
-- }}}

runVisitorThroughCheckpoint :: -- {{{
    Monoid α ⇒
    Checkpoint →
    Visitor α →
    α
runVisitorThroughCheckpoint = runIdentity .* runVisitorTThroughCheckpoint
-- }}}

runVisitorTThroughCheckpoint :: -- {{{
    (Monad m, Monoid α) ⇒
    Checkpoint →
    VisitorT m α →
    m α
runVisitorTThroughCheckpoint = go mempty .* initialVisitorState
  where
    go !accum =
        stepVisitorTThroughCheckpoint
        >=>
        \(maybe_solution,maybe_new_visitor_state) →
            let new_accum = maybe id (flip mappend) maybe_solution accum
            in maybe (return new_accum) (go new_accum) maybe_new_visitor_state
{-# INLINE runVisitorTThroughCheckpoint #-}
-- }}}

stepVisitorThroughCheckpoint :: -- {{{
    VisitorState α →
    (Maybe α,Maybe (VisitorState α))
stepVisitorThroughCheckpoint = runIdentity . stepVisitorTThroughCheckpoint
-- }}}

stepVisitorTThroughCheckpoint :: -- {{{
    Monad m ⇒
    VisitorTState m α →
    m (Maybe α,Maybe (VisitorTState m α))
stepVisitorTThroughCheckpoint visitor_state@(VisitorTState context checkpoint visitor) = case checkpoint of
    Explored → return (Nothing, moveUpContext)
    Unexplored → getView >>= \view → case view of
        Return x → return (Just x, moveUpContext)
        Null :>>= _ → return (Nothing, moveUpContext)
        Cache mx :>>= k →
            mx >>= return . maybe
                (Nothing, moveUpContext)
                (\x → (Nothing, Just $
                    VisitorTState
                        (context |> CacheContextStep (encode x))
                        Unexplored
                        (VisitorT . k $ x)
                ))
        Choice left right :>>= k → return
            (Nothing, Just $
                VisitorTState
                    (context |> LeftBranchContextStep Unexplored (right >>= VisitorT . k))
                    Unexplored
                    (left >>= VisitorT . k)
            )
    CacheCheckpoint cache rest_checkpoint → getView >>= \view → case view of
        Cache mx :>>= k → return
            (Nothing, Just $
                VisitorTState
                    (context |> CacheContextStep cache)
                    rest_checkpoint
                    (either error (VisitorT . k) . decode $ cache)
            )
        _ → throw PastVisitorIsInconsistentWithPresentVisitor
    ChoiceCheckpoint left_checkpoint right_checkpoint →  getView >>= \view → case view of
        Choice left right :>>= k → return
            (Nothing, Just $
                VisitorTState
                    (context |> LeftBranchContextStep right_checkpoint (right >>= VisitorT . k))
                    left_checkpoint
                    (left >>= VisitorT . k)
            )
        _ → throw PastVisitorIsInconsistentWithPresentVisitor
  where
    getView = viewT . unwrapVisitorT $ visitor
    moveUpContext = go context
      where
        go context = case viewr context of
            EmptyR → Nothing
            rest_context :> LeftBranchContextStep right_checkpoint right_visitor →
                Just (VisitorTState
                        (rest_context |> RightBranchContextStep)
                        right_checkpoint
                        right_visitor
                     )
            rest_context :> _ → go rest_context
{-# INLINE stepVisitorTThroughCheckpoint #-}
-- }}}

walkVisitor :: -- {{{
    Monoid α ⇒
    Visitor α →
    [(α,Checkpoint)]
walkVisitor = walkVisitorThroughCheckpoint Unexplored
-- }}}

walkVisitorT :: -- {{{
    (Monad m, Monoid α) ⇒
    VisitorT m α →
    ResultFetcher m α
walkVisitorT = walkVisitorTThroughCheckpoint Unexplored
{-# INLINE walkVisitorT #-}
-- }}}

walkVisitorThroughCheckpoint :: -- {{{
    Monoid α ⇒
    Checkpoint →
    Visitor α →
    [(α,Checkpoint)]
walkVisitorThroughCheckpoint = go1 .* walkVisitorTThroughCheckpoint
  where
    go1 (runIdentity . fetchResult → Just (next_accum,checkpoint,next_result)) = go3 next_accum checkpoint next_result
    go1 _ = [(mempty,Explored)]

    go2 (runIdentity . fetchResult → Just (next_accum,checkpoint,next_result)) = go3 next_accum checkpoint next_result
    go2 _ = []

    go3 next_accum checkpoint !next_result = (next_accum,checkpoint):go2 next_result
-- }}}

walkVisitorTThroughCheckpoint :: -- {{{
    ∀ m α. (Monad m, Monoid α) ⇒
    Checkpoint →
    VisitorT m α →
    ResultFetcher m α
walkVisitorTThroughCheckpoint = go mempty .* initialVisitorState
  where
    go :: α → VisitorTState m α → ResultFetcher m α
    go accum visitor_state = ResultFetcher $
        stepVisitorTThroughCheckpoint visitor_state
        >>=
        \(maybe_solution,maybe_new_state) → return $
            let !new_accum = maybe id (flip mappend) maybe_solution accum
            in Just $ case maybe_new_state of
                Nothing → (new_accum,Explored,ResultFetcher (return Nothing))
                Just new_state@(VisitorTState context unexplored_checkpoint _) →
                    (new_accum
                    ,checkpointFromContext context unexplored_checkpoint
                    ,go new_accum new_state
                    )
{-# INLINE walkVisitorTThroughCheckpoint #-}
-- }}}

-- }}}
