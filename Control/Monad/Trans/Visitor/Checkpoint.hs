-- Language extensions {{{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- }}}

module Control.Monad.Trans.Visitor.Checkpoint where

-- Imports {{{
import Control.Exception (Exception(),throw)
import Control.Monad ((>=>),join)
import Control.Monad.Operational (ProgramViewT(..),viewT)
import Control.Monad.Trans.Class (MonadTrans(..))

import Data.ByteString (ByteString)
import Data.Composition
import Data.Functor.Identity (Identity,runIdentity)
import Data.Maybe (mapMaybe)
import Data.Monoid (Monoid(..))
import Data.Monoid.Unicode
import Data.Sequence ((|>),Seq,viewl,ViewL(..),viewr,ViewR(..))
import qualified Data.Sequence as Seq
import Data.Serialize (Serialize(),decode,encode)
import Data.Typeable (Typeable)

import Control.Monad.Trans.Visitor
import Control.Monad.Trans.Visitor.Path
-- }}}

-- Types {{{

data VisitorCheckpoint = -- {{{
    CacheCheckpoint ByteString VisitorCheckpoint
  | ChoiceCheckpoint VisitorCheckpoint VisitorCheckpoint
  | Explored
  | Unexplored
  deriving (Eq,Read,Show)
-- }}}

type VisitorCheckpointCursor = Seq VisitorCheckpointDifferential

data VisitorCheckpointDifferential = -- {{{
    CacheCheckpointD ByteString
  | ChoiceCheckpointD Branch VisitorCheckpoint
  deriving (Eq,Read,Show)
-- }}}

data VisitorStatusUpdate α = VisitorStatusUpdate -- {{{
    {   visitorStatusCheckpoint :: VisitorCheckpoint
    ,   visitorStatusNewResults :: α
    } deriving (Eq,Show)
-- }}}

type VisitorTContext m α = Seq (VisitorTContextStep m α)
type VisitorContext α = VisitorTContext Identity α

data VisitorTContextStep m α = -- {{{
    BranchContextStep Branch
  | CacheContextStep ByteString
  | LeftChoiceContextStep VisitorCheckpoint (VisitorT m α)
-- }}}
type VisitorContextStep = VisitorTContextStep Identity

type VisitorTContextUpdate m α = -- {{{
    VisitorTContext m α →
    Maybe (VisitorTContext m α, VisitorCheckpoint, VisitorT m α)
-- }}}

type VisitorContextUpdate α = VisitorTContextUpdate Identity α

newtype VisitorTResultFetcher m α = VisitorTResultFetcher -- {{{
    {   fetchVisitorTResult :: m (Maybe (α, VisitorCheckpoint, VisitorTResultFetcher m α))
    }
-- }}}

type VisitorResultFetcher = VisitorTResultFetcher Identity

-- }}}

-- Exceptions {{{

data InconsistentCheckpoints = InconsistentCheckpoints VisitorCheckpoint VisitorCheckpoint deriving (Eq,Show,Typeable)

instance Exception InconsistentCheckpoints

-- }}}

-- Instances {{{

instance Monoid VisitorCheckpoint where -- {{{
    mempty = Unexplored
    Explored `mappend` _ = Explored
    _ `mappend` Explored = Explored
    Unexplored `mappend` x = x
    x `mappend` Unexplored = x
    (ChoiceCheckpoint lx rx) `mappend` (ChoiceCheckpoint ly ry) = mergeCheckpointRoot (ChoiceCheckpoint (lx ⊕ ly) (rx ⊕ ry))
    (CacheCheckpoint cx x) `mappend` (CacheCheckpoint cy y)
      | cx == cy = mergeCheckpointRoot (CacheCheckpoint cx (x ⊕ y))
    mappend x y = throw (InconsistentCheckpoints x y)
-- }}}

instance Monoid α ⇒ Monoid (VisitorStatusUpdate α) where -- {{{
    mempty = VisitorStatusUpdate mempty mempty
    VisitorStatusUpdate a1 b1 `mappend` VisitorStatusUpdate a2 b2 = VisitorStatusUpdate (a1 `mappend` a2) (b1 `mappend` b2)
-- }}}

instance Show (VisitorTContextStep m α) where -- {{{
    show (BranchContextStep branch) = "BranchContextStep{" ++ show branch ++ "}"
    show (CacheContextStep c) = "CacheContextStep[" ++ show c ++ "]"
    show (LeftChoiceContextStep checkpoint _) = "LeftChoiceContextStep(" ++ show checkpoint ++ ")"
-- }}}

-- }}}

-- Functions {{{

checkpointFromContext :: VisitorTContext m α → VisitorCheckpoint → VisitorCheckpoint -- {{{
checkpointFromContext = checkpointFromSequence $
    \step → case step of
        BranchContextStep LeftBranch → flip ChoiceCheckpoint Explored
        BranchContextStep RightBranch → ChoiceCheckpoint Explored
        CacheContextStep cache → CacheCheckpoint cache
        LeftChoiceContextStep right_checkpoint _ → flip ChoiceCheckpoint right_checkpoint
-- }}}

checkpointFromCursor :: VisitorCheckpointCursor → VisitorCheckpoint → VisitorCheckpoint -- {{{
checkpointFromCursor = checkpointFromSequence $
    \step → case step of
        CacheCheckpointD cache → CacheCheckpoint cache
        ChoiceCheckpointD LeftBranch right_checkpoint → flip ChoiceCheckpoint right_checkpoint
        ChoiceCheckpointD RightBranch left_checkpoint → ChoiceCheckpoint left_checkpoint
-- }}}

checkpointFromInitialPath :: VisitorPath → VisitorCheckpoint → VisitorCheckpoint -- {{{
checkpointFromInitialPath = checkpointFromSequence $
    \step → case step of
        CacheStep c → CacheCheckpoint c
        ChoiceStep LeftBranch → flip ChoiceCheckpoint Unexplored
        ChoiceStep RightBranch → ChoiceCheckpoint Unexplored
-- }}}

checkpointFromSequence :: -- {{{
    (α → (VisitorCheckpoint → VisitorCheckpoint)) →
    Seq α →
    VisitorCheckpoint →
    VisitorCheckpoint
checkpointFromSequence processStep (viewr → EmptyR) = id
checkpointFromSequence processStep (viewr → rest :> step) =
    checkpointFromSequence processStep rest
    .
    mergeCheckpointRoot
    .
    processStep step
-- }}}

checkpointFromUnexploredPath :: VisitorPath → VisitorCheckpoint -- {{{
checkpointFromUnexploredPath (viewl → EmptyL) = Unexplored
checkpointFromUnexploredPath (viewl → step :< rest_path) =
    case step of
        CacheStep c → CacheCheckpoint c
        ChoiceStep LeftBranch → flip ChoiceCheckpoint Explored
        ChoiceStep RightBranch → ChoiceCheckpoint Explored
    $
    checkpointFromUnexploredPath rest_path
-- }}}

gatherResults :: -- {{{
    (Functor m, Monad m, Monoid α) ⇒
    VisitorTResultFetcher m α →
    m α
gatherResults = go mempty
  where
    go result =
        fetchVisitorTResult
        >=>
        maybe
            (return result)
            (\(result,_,fetcher) → go result fetcher)
-- }}}

mergeAllCheckpointNodes :: VisitorCheckpoint → VisitorCheckpoint -- {{{
mergeAllCheckpointNodes (ChoiceCheckpoint left right) = mergeCheckpointRoot (ChoiceCheckpoint (mergeAllCheckpointNodes left) (mergeAllCheckpointNodes right))
mergeAllCheckpointNodes (CacheCheckpoint cache checkpoint) = mergeCheckpointRoot (CacheCheckpoint cache (mergeAllCheckpointNodes checkpoint))
mergeAllCheckpointNodes checkpoint = checkpoint
-- }}}

mergeCheckpointRoot :: VisitorCheckpoint → VisitorCheckpoint -- {{{
mergeCheckpointRoot (ChoiceCheckpoint Unexplored Unexplored) = Unexplored
mergeCheckpointRoot (ChoiceCheckpoint Explored Explored) = Explored
mergeCheckpointRoot (CacheCheckpoint _ Explored) = Explored
mergeCheckpointRoot checkpoint = checkpoint
-- }}}

moveDownContext :: -- {{{
    VisitorTContextStep m α →
    VisitorCheckpoint →
    VisitorT m α →
    VisitorTContextUpdate m α
moveDownContext step checkpoint visitor =
    Just
    .
    (,checkpoint,visitor)
    .
    (|> step)
-- }}}

moveUpContext :: VisitorTContextUpdate m α -- {{{
moveUpContext (viewr → EmptyR) = Nothing
moveUpContext (viewr → rest_context :> BranchContextStep _) = moveUpContext rest_context
moveUpContext (viewr → rest_context :> CacheContextStep _) = moveUpContext rest_context
moveUpContext (viewr → rest_context :> LeftChoiceContextStep right_checkpoint right_visitor) =
    Just (rest_context |> BranchContextStep RightBranch
         ,right_checkpoint
         ,right_visitor
         )
-- }}}

pathFromContext :: VisitorTContext m α → VisitorPath -- {{{
pathFromContext = fmap pathStepFromContextStep
-- }}}

pathFromCursor :: VisitorCheckpointCursor → VisitorPath -- {{{
pathFromCursor = fmap pathStepFromCursorDifferential
-- }}}

pathStepFromContextStep :: VisitorTContextStep m α → VisitorStep -- {{{
pathStepFromContextStep (BranchContextStep active_branch) = ChoiceStep active_branch
pathStepFromContextStep (CacheContextStep cache) = CacheStep cache
pathStepFromContextStep (LeftChoiceContextStep _ _) = ChoiceStep LeftBranch
-- }}}

pathStepFromCursorDifferential :: VisitorCheckpointDifferential → VisitorStep -- {{{
pathStepFromCursorDifferential (CacheCheckpointD cache) = CacheStep cache
pathStepFromCursorDifferential (ChoiceCheckpointD active_branch _) = ChoiceStep active_branch
-- }}}

runVisitorThroughCheckpoint :: -- {{{
    Monoid α ⇒
    VisitorCheckpoint →
    Visitor α →
    [(α,VisitorCheckpoint)]
runVisitorThroughCheckpoint = go True .* runVisitorTThroughCheckpoint
  where
    go True (runIdentity . fetchVisitorTResult → Nothing) = [(mempty,Explored)]
    go False (runIdentity . fetchVisitorTResult → Nothing) = []
    go _ (runIdentity . fetchVisitorTResult → Just (next_accum,checkpoint,next_result)) =
      (next_accum,checkpoint)
      :
      go False next_result
-- }}}

runVisitorThroughCheckpointAndGatherResults :: -- {{{
    Monoid α ⇒
    VisitorCheckpoint →
    Visitor α →
    α
runVisitorThroughCheckpointAndGatherResults = (fst . last) .* runVisitorThroughCheckpoint
-- }}}

runVisitorTThroughCheckpoint :: -- {{{
    (Functor m, Monad m, Monoid α) ⇒
    VisitorCheckpoint →
    VisitorT m α →
    VisitorTResultFetcher m α
runVisitorTThroughCheckpoint = go mempty Seq.empty
  where
    go accum context =
        (VisitorTResultFetcher
         .
         fmap (\x → case x of
            (maybe_solution,Nothing) → Just
                (maybe id (flip mappend) maybe_solution accum
                ,Explored
                ,VisitorTResultFetcher (return Nothing)
                )
            (maybe_solution,Just (new_context, new_unexplored_checkpoint, new_visitor)) → Just $
              let new_accum = maybe id (flip mappend) maybe_solution accum in
                (new_accum
                ,checkpointFromContext new_context new_unexplored_checkpoint
                ,go new_accum new_context new_unexplored_checkpoint new_visitor
                )
         )
        )
        .*
        stepVisitorTThroughCheckpoint context
-- }}}

runVisitorTThroughCheckpointAndGatherResults :: -- {{{
    (Functor m, Monad m, Monoid α) ⇒
    VisitorCheckpoint →
    VisitorT m α →
    m α
runVisitorTThroughCheckpointAndGatherResults = gatherResults .* runVisitorTThroughCheckpoint
-- }}}

runVisitorTTWithCheckpoints :: -- {{{
    (Functor m, Monad m, Monoid α) ⇒
    VisitorT m α →
    VisitorTResultFetcher m α
runVisitorTTWithCheckpoints = runVisitorTThroughCheckpoint Unexplored
-- }}}

runVisitorWithCheckpoints :: -- {{{
    Monoid α ⇒
    Visitor α →
    [(α,VisitorCheckpoint)]
runVisitorWithCheckpoints = runVisitorThroughCheckpoint Unexplored
-- }}}

stepVisitorThroughCheckpoint :: -- {{{
    VisitorContext α →
    VisitorCheckpoint →
    Visitor α →
    (Maybe α,Maybe (VisitorContext α, VisitorCheckpoint, Visitor α))
stepVisitorThroughCheckpoint context checkpoint = runIdentity . stepVisitorTThroughCheckpoint context checkpoint
-- }}}

stepVisitorTThroughCheckpoint :: -- {{{
    (Functor m, Monad m) ⇒
    VisitorTContext m α →
    VisitorCheckpoint →
    VisitorT m α →
    m (Maybe α,Maybe (VisitorTContext m α, VisitorCheckpoint, VisitorT m α))
stepVisitorTThroughCheckpoint context Explored = const $ return (Nothing,moveUpContext context)
stepVisitorTThroughCheckpoint context checkpoint = viewT . unwrapVisitorT >=> \view → case (view,checkpoint) of
    (Return x,Unexplored) → return (Just x, moveUpMyContext)
    (Null :>>= _,Unexplored) → return (Nothing, moveUpMyContext)
    (Cache mx :>>= k,CacheCheckpoint cache rest_checkpoint) →
        return
        .
        (Nothing,)
        .
        moveDownMyContext (CacheContextStep cache) rest_checkpoint
        .
        either error (VisitorT . k)
        .
        decode
        $
        cache
    (Cache mx :>>= k,Unexplored) →
        mx >>= \x → return . (Nothing,) $
        moveDownMyContext
            (CacheContextStep (encode x))
            Unexplored
            (VisitorT . k $ x)
    (Choice left right :>>= k, ChoiceCheckpoint left_checkpoint right_checkpoint) → return
        (Nothing
        ,moveDownMyContext
            (LeftChoiceContextStep right_checkpoint (right >>= VisitorT . k))
            left_checkpoint
            (left >>= VisitorT . k)
        )
    (Choice left right :>>= k,Unexplored) → return
        (Nothing
        ,moveDownMyContext
            (LeftChoiceContextStep Unexplored (right >>= VisitorT . k))
            Unexplored
            (left >>= VisitorT . k)
        )
    _ → throw PastVisitorIsInconsistentWithPresentVisitor
  where
    moveUpMyContext = moveUpContext context
    moveDownMyContext step checkpoint visitor = moveDownContext step checkpoint visitor context
-- }}}

-- }}}
