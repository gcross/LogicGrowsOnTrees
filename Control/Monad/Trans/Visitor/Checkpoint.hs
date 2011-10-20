-- @+leo-ver=5-thin
-- @+node:gcross.20110923120247.1191: * @file Control/Monad/Trans/Visitor/Checkpoint.hs
-- @@language haskell

-- @+<< Language extensions >>
-- @+node:gcross.20110923120247.1192: ** << Language extensions >>
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- @-<< Language extensions >>

module Control.Monad.Trans.Visitor.Checkpoint where

-- @+<< Import needed modules >>
-- @+node:gcross.20110923120247.1193: ** << Import needed modules >>
import Control.Exception (Exception(),throw)
import Control.Monad ((>=>),join)
import Control.Monad.Operational (ProgramViewT(..),viewT)
import Control.Monad.Trans.Class (MonadTrans(..))

import Data.ByteString (ByteString)
import Data.Composition
import Data.Functor.Identity (Identity,runIdentity)
import Data.Sequence ((|>),Seq,viewl,ViewL(..),viewr,ViewR(..))
import qualified Data.Sequence as Seq
import Data.Serialize (Serialize(),decode,encode)
import Data.Typeable (Typeable)

import Control.Monad.Trans.Visitor
import Control.Monad.Trans.Visitor.Path
-- @-<< Import needed modules >>

-- @+others
-- @+node:gcross.20110923120247.1194: ** Types
-- @+node:gcross.20110923120247.1195: *3* VisitorCheckpoint
data VisitorCheckpoint =
    BranchCheckpoint WhichBranchActive VisitorCheckpoint
  | CacheCheckpoint ByteString VisitorCheckpoint
  | ChoiceCheckpoint VisitorCheckpoint VisitorCheckpoint
  | Unexplored
  deriving (Eq,Read,Show)
-- @+node:gcross.20111020151748.1287: *3* VisitorCheckpointCursor
type VisitorCheckpointCursor = Seq VisitorCheckpointDifferential
-- @+node:gcross.20111020151748.1286: *3* VisitorCheckpointDifferential
data VisitorCheckpointDifferential =
    BranchCheckpointD WhichBranchActive
  | CacheCheckpointD ByteString
  | ChoiceCheckpointD WhichBranchActive VisitorCheckpoint
  deriving (Eq,Read,Show)
-- @+node:gcross.20110923164140.1178: *3* VisitorTContext
type VisitorTContext m α = Seq (VisitorTContextStep m α)
type VisitorContext α = VisitorTContext Identity α
-- @+node:gcross.20110923120247.1196: *3* VisitorTContextStep
data VisitorTContextStep m α =
    BranchContextStep WhichBranchActive
  | CacheContextStep ByteString
  | LeftChoiceContextStep VisitorCheckpoint (VisitorT m α)

type VisitorContextStep = VisitorTContextStep Identity
-- @+node:gcross.20110923164140.1238: *3* VisitorTContextUpdate
data VisitorTContextUpdate m α =
    MoveUpContext
  | MoveDownContext (VisitorTContextStep m α) VisitorCheckpoint (VisitorT m α)

type VisitorContextUpdate = VisitorTContextUpdate Identity
-- @+node:gcross.20111019113757.1408: *3* VisitorTResultFetcher
newtype VisitorTResultFetcher m α = VisitorTResultFetcher
    {   fetchVisitorTResult :: m (Maybe (Maybe (VisitorSolution α), VisitorCheckpoint, VisitorTResultFetcher m α))
    }

type VisitorResultFetcher = VisitorTResultFetcher Identity
-- @+node:gcross.20110923164140.1179: ** Functions
-- @+node:gcross.20111020151748.1288: *3* applyCheckpointCursorToLabel
applyCheckpointCursorToLabel :: VisitorCheckpointCursor → VisitorLabel → VisitorLabel
applyCheckpointCursorToLabel (viewl → EmptyL) = id
applyCheckpointCursorToLabel (viewl → step :< rest) =
    applyCheckpointCursorToLabel rest
    .
    case step of
        BranchCheckpointD active_branch → labelTransformerForBranch active_branch
        CacheCheckpointD _ → id
        ChoiceCheckpointD active_branch _ → labelTransformerForBranch active_branch
-- @+node:gcross.20110923164140.1240: *3* applyContextUpdate
applyContextUpdate ::
    VisitorTContextUpdate m α →
    VisitorTContext m α →
    Maybe (VisitorTContext m α, VisitorCheckpoint, VisitorT m α)
applyContextUpdate MoveUpContext = moveUpContext
applyContextUpdate (MoveDownContext step checkpoint v) =
    Just
    .
    (,checkpoint,v)
    .
    (|> step)
-- @+node:gcross.20111019113757.1400: *3* applyContextToLabel
applyContextToLabel :: VisitorTContext m α → VisitorLabel → VisitorLabel
applyContextToLabel (viewl → EmptyL) = id
applyContextToLabel (viewl → step :< rest) =
    applyContextToLabel rest
    .
    case step of
        BranchContextStep branch_active → labelTransformerForBranch branch_active
        CacheContextStep _ → id
        LeftChoiceContextStep _ _ → leftChildLabel
-- @+node:gcross.20110923164140.1182: *3* checkpointFromContext
checkpointFromContext :: VisitorTContext m α → VisitorCheckpoint → VisitorCheckpoint
checkpointFromContext (viewl → EmptyL) = id
checkpointFromContext (viewl → differential :< rest) =
    case differential of
        BranchContextStep active_branch → BranchCheckpoint active_branch
        CacheContextStep cache → CacheCheckpoint cache
        LeftChoiceContextStep right_checkpoint _ → flip ChoiceCheckpoint right_checkpoint
    .
    checkpointFromContext rest
-- @+node:gcross.20111020182554.1265: *3* checkpointFromCursor
checkpointFromCursor :: VisitorCheckpointCursor → VisitorCheckpoint → VisitorCheckpoint
checkpointFromCursor (viewl → EmptyL) = id
checkpointFromCursor (viewl → step :< rest) =
    case step of
        BranchCheckpointD active_branch → BranchCheckpoint active_branch
        CacheCheckpointD cache → CacheCheckpoint cache
        ChoiceCheckpointD LeftBranchActive right_checkpoint → flip ChoiceCheckpoint right_checkpoint
        ChoiceCheckpointD RightBranchActive left_checkpoint → ChoiceCheckpoint left_checkpoint
    .
    checkpointFromCursor rest
-- @+node:gcross.20111019113757.1412: *3* labelFromContext
labelFromContext :: VisitorTContext m α → VisitorLabel
labelFromContext = flip applyContextToLabel rootLabel
-- @+node:gcross.20110923164140.1239: *3* moveUpContext
moveUpContext :: VisitorTContext m α → Maybe (VisitorTContext m α, VisitorCheckpoint, VisitorT m α)
moveUpContext (viewr → EmptyR) = Nothing
moveUpContext (viewr → rest_context :> BranchContextStep _) = moveUpContext rest_context
moveUpContext (viewr → rest_context :> CacheContextStep _) = moveUpContext rest_context
moveUpContext (viewr → rest_context :> LeftChoiceContextStep right_checkpoint right_visitor) =
    Just (rest_context |> BranchContextStep RightBranchActive
         ,right_checkpoint
         ,right_visitor
         )
-- @+node:gcross.20110923164140.1265: *3* pathFromContext
pathFromContext :: VisitorTContext m α → VisitorPath
pathFromContext = fmap pathStepFromContextStep
-- @+node:gcross.20110923164140.1266: *3* pathStepFromContextStep
pathStepFromContextStep :: VisitorTContextStep m α → VisitorStep
pathStepFromContextStep (BranchContextStep active_branch) = ChoiceStep active_branch
pathStepFromContextStep (CacheContextStep cache) = CacheStep cache
pathStepFromContextStep (LeftChoiceContextStep _ _) = ChoiceStep LeftBranchActive
-- @+node:gcross.20110923164140.1246: *3* runVisitorThroughCheckpoint
runVisitorThroughCheckpoint ::
    VisitorCheckpoint →
    Visitor α →
    [(Maybe (VisitorSolution α),VisitorCheckpoint)]
runVisitorThroughCheckpoint checkpoint visitor = go (runVisitorTThroughCheckpoint checkpoint visitor)
  where
    go (runIdentity . fetchVisitorTResult → Nothing) = []
    go (runIdentity . fetchVisitorTResult → Just (maybe_solution,checkpoint,next_result)) =
      (maybe_solution,checkpoint)
      :
      go next_result
-- @+node:gcross.20110923164140.1244: *3* runVisitorTThroughCheckpoint
runVisitorTThroughCheckpoint ::
    (Functor m, Monad m) ⇒
    VisitorCheckpoint →
    VisitorT m α →
    VisitorTResultFetcher m α
runVisitorTThroughCheckpoint = go Seq.empty
  where
    go context =
        (VisitorTResultFetcher
         .
         fmap (\(maybe_solution, update) →
            case applyContextUpdate update context of
                Nothing → Nothing
                Just (new_context, new_unexplored_checkpoint, new_visitor) → Just
                    (fmap (
                        VisitorSolution
                        .
                        labelFromContext
                        $
                        context
                     ) maybe_solution
                    ,checkpointFromContext new_context new_unexplored_checkpoint
                    ,go new_context new_unexplored_checkpoint new_visitor
                    )
         )

        )
        .*
        stepVisitorTThroughCheckpoint
-- @+node:gcross.20110923164140.1242: *3* stepVisitorThroughCheckpoint
stepVisitorThroughCheckpoint ::
    VisitorCheckpoint →
    Visitor α →
    (Maybe α, VisitorContextUpdate α)
stepVisitorThroughCheckpoint checkpoint = runIdentity . stepVisitorTThroughCheckpoint checkpoint
-- @+node:gcross.20110923164140.1186: *3* stepVisitorTThroughCheckpoint
stepVisitorTThroughCheckpoint ::
    Monad m ⇒
    VisitorCheckpoint →
    VisitorT m α →
    m (Maybe α, VisitorTContextUpdate m α)
stepVisitorTThroughCheckpoint checkpoint = viewT . unwrapVisitorT >=> \view →  case view of
    Return x → return (Just x, MoveUpContext)
    Null :>>= _ → return (Nothing, MoveUpContext)
    Cache mx :>>= k →
        case checkpoint of
            CacheCheckpoint cache rest_checkpoint → return
                (Nothing
                ,MoveDownContext (CacheContextStep cache) rest_checkpoint $ either error (VisitorT . k) (decode cache)
                )
            Unexplored → do
                x ← mx
                return
                    (Nothing
                    ,MoveDownContext (CacheContextStep (encode x)) Unexplored ((VisitorT . k) x)
                    )
            _ → throw ChoiceStepAtCachePoint
    Choice left right :>>= k → return 
        (Nothing
        ,case checkpoint of
            BranchCheckpoint LeftBranchActive left_checkpoint →
                MoveDownContext (BranchContextStep LeftBranchActive) left_checkpoint (left >>= VisitorT . k)
            BranchCheckpoint RightBranchActive right_checkpoint →
                MoveDownContext (BranchContextStep RightBranchActive) right_checkpoint (right >>= VisitorT . k)
            ChoiceCheckpoint left_checkpoint right_checkpoint →
                MoveDownContext (LeftChoiceContextStep right_checkpoint (right >>= VisitorT . k)) left_checkpoint (left >>= VisitorT . k)
            Unexplored →
                MoveDownContext (LeftChoiceContextStep Unexplored (right >>= VisitorT . k)) Unexplored (left >>= VisitorT . k)
            _ → throw CacheStepAtChoicePoint
        )
-- @-others
-- @-leo
