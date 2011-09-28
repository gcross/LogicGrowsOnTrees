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
    BranchCheckpoint Bool VisitorCheckpoint
  | CacheCheckpoint ByteString VisitorCheckpoint
  | ChoiceCheckpoint VisitorCheckpoint VisitorCheckpoint
  | Unexplored
  deriving (Eq,Read,Show)
-- @+node:gcross.20110923120247.1196: *3* VisitorTCheckpointDifferential
data VisitorTCheckpointDifferential m α =
    BranchCheckpointD Bool
  | CacheCheckpointD ByteString
  | ChoiceCheckpointD Bool VisitorCheckpoint (VisitorT m α)

type VisitorCheckpointDifferential = VisitorTCheckpointDifferential Identity
-- @+node:gcross.20110923164140.1178: *3* VisitorTCheckpointContext
type VisitorTCheckpointContext m α = Seq (VisitorTCheckpointDifferential m α)
type VisitorCheckpointContext α = VisitorTCheckpointContext Identity α
-- @+node:gcross.20110923164140.1238: *3* VisitorTCheckpointContextMove
data VisitorTCheckpointContextMove m α =
    MoveUpContext
  | MoveDownContext (VisitorTCheckpointDifferential m α) VisitorCheckpoint (VisitorT m α)

type VisitorCheckpointContextMove = VisitorTCheckpointContextMove Identity
-- @+node:gcross.20110923164140.1179: ** Functions
-- @+node:gcross.20110923164140.1240: *3* applyContextMove
applyContextMove ::
    VisitorTCheckpointContextMove m α →
    VisitorTCheckpointContext m α →
    Maybe (VisitorTCheckpointContext m α, VisitorCheckpoint, VisitorT m α)
applyContextMove MoveUpContext = moveUpContext
applyContextMove (MoveDownContext differential checkpoint v) =
    Just
    .
    (,checkpoint,v)
    .
    (|> differential)
-- @+node:gcross.20110923164140.1182: *3* checkpointFromContext
checkpointFromContext :: VisitorTCheckpointContext m α → VisitorCheckpoint
checkpointFromContext (viewl → EmptyL) = Unexplored
checkpointFromContext (viewl → differential :< rest) =
    let rest_checkpoint = checkpointFromContext rest
    in case differential of
        BranchCheckpointD which_explored → BranchCheckpoint which_explored rest_checkpoint
        CacheCheckpointD cache → CacheCheckpoint cache rest_checkpoint
        ChoiceCheckpointD False right_checkpoint _ → ChoiceCheckpoint rest_checkpoint right_checkpoint
        ChoiceCheckpointD True left_checkpoint _ → ChoiceCheckpoint left_checkpoint rest_checkpoint
-- @+node:gcross.20110923164140.1239: *3* moveUpContext
moveUpContext :: VisitorTCheckpointContext m α → Maybe (VisitorTCheckpointContext m α, VisitorCheckpoint, VisitorT m α)
moveUpContext (viewr → EmptyR) = Nothing
moveUpContext (viewr → rest_context :> BranchCheckpointD _) = moveUpContext rest_context
moveUpContext (viewr → rest_context :> CacheCheckpointD _) = moveUpContext rest_context
moveUpContext (viewr → rest_context :> ChoiceCheckpointD which_explored other_checkpoint other) =
    Just (rest_context |> BranchCheckpointD which_explored
         ,other_checkpoint
         ,other
         )
-- @+node:gcross.20110923164140.1265: *3* pathFromContext
pathFromContext :: VisitorTCheckpointContext m α → VisitorPath
pathFromContext = fmap pathStepFromDifferential
-- @+node:gcross.20110923164140.1266: *3* pathStepFromDifferential
pathStepFromDifferential :: VisitorTCheckpointDifferential m α → VisitorStep
pathStepFromDifferential (BranchCheckpointD which_explored) = ChoiceStep (not which_explored)
pathStepFromDifferential (CacheCheckpointD cache) = CacheStep cache
pathStepFromDifferential (ChoiceCheckpointD which_explored _ _) = ChoiceStep (not which_explored)
-- @+node:gcross.20110923164140.1246: *3* runVisitorThroughCheckpoint
runVisitorThroughCheckpoint ::
    Monad m ⇒
    (α → m β) →
    (VisitorCheckpointContextMove α → m (Maybe (VisitorCheckpoint, Visitor α))) →
    VisitorCheckpoint →
    Visitor α →
    m ()
runVisitorThroughCheckpoint acceptSolution updateCheckpointContext checkpoint v =
    let (maybe_solution, move) = stepVisitorThroughCheckpoint checkpoint v
    in  maybe (return undefined) acceptSolution maybe_solution
        >>
        updateCheckpointContext move
        >>=
        maybe (return ()) (uncurry $ runVisitorThroughCheckpoint acceptSolution updateCheckpointContext)
-- @+node:gcross.20110923164140.1244: *3* runVisitorTThroughCheckpoint
runVisitorTThroughCheckpoint ::
    (Monad m, Monad (t m), MonadTrans t) ⇒
    (α → t m β) →
    (VisitorTCheckpointContextMove m α → t m (Maybe (VisitorCheckpoint, VisitorT m α))) →
    VisitorCheckpoint →
    VisitorT m α →
    t m ()
runVisitorTThroughCheckpoint acceptSolution updateCheckpointContext checkpoint v =
    (lift $ stepVisitorTThroughCheckpoint checkpoint v)
    >>= \(maybe_solution, move) →
    maybe (return undefined) acceptSolution maybe_solution
    >>
    updateCheckpointContext move
    >>=
    maybe (return ()) (uncurry $ runVisitorTThroughCheckpoint acceptSolution updateCheckpointContext)
-- @+node:gcross.20110923164140.1248: *3* runVisitorTThroughCheckpointUsingOwnMonad
runVisitorTThroughCheckpointUsingOwnMonad ::
    Monad m ⇒
    (α → m β) →
    (VisitorTCheckpointContextMove m α → m (Maybe (VisitorCheckpoint, VisitorT m α))) →
    VisitorCheckpoint →
    VisitorT m α →
    m ()
runVisitorTThroughCheckpointUsingOwnMonad acceptSolution updateCheckpointContext checkpoint v =
    stepVisitorTThroughCheckpoint checkpoint v
    >>= \(maybe_solution, move) →
    maybe (return undefined) acceptSolution maybe_solution
    >>
    updateCheckpointContext move
    >>=
    maybe (return ()) (uncurry $ runVisitorTThroughCheckpointUsingOwnMonad acceptSolution updateCheckpointContext)
-- @+node:gcross.20110923164140.1242: *3* stepVisitorThroughCheckpoint
stepVisitorThroughCheckpoint ::
    VisitorCheckpoint →
    Visitor α →
    (Maybe α, VisitorCheckpointContextMove α)
stepVisitorThroughCheckpoint checkpoint = runIdentity . stepVisitorTThroughCheckpoint checkpoint
-- @+node:gcross.20110923164140.1186: *3* stepVisitorTThroughCheckpoint
stepVisitorTThroughCheckpoint ::
    Monad m ⇒
    VisitorCheckpoint →
    VisitorT m α →
    m (Maybe α, VisitorTCheckpointContextMove m α)
stepVisitorTThroughCheckpoint checkpoint = viewT . unwrapVisitorT >=> \view →  case view of
    Return x → return (Just x, MoveUpContext)
    Null :>>= _ → return (Nothing, MoveUpContext)
    Cache mx :>>= k →
        case checkpoint of
            CacheCheckpoint cache rest_checkpoint → return
                (Nothing
                ,MoveDownContext (CacheCheckpointD cache) rest_checkpoint $ either error (VisitorT . k) (decode cache)
                )
            Unexplored → do
                x ← mx
                return
                    (Nothing
                    ,MoveDownContext (CacheCheckpointD (encode x)) Unexplored ((VisitorT . k) x)
                    )
            _ → throw ChoiceStepAtCachePoint
    Choice left right :>>= k → return 
        (Nothing
        ,case checkpoint of
            BranchCheckpoint False right_checkpoint →
                MoveDownContext (BranchCheckpointD False) right_checkpoint (right >>= VisitorT . k)
            BranchCheckpoint True left_checkpoint →
                MoveDownContext (BranchCheckpointD True) left_checkpoint (left >>= VisitorT . k)
            ChoiceCheckpoint left_checkpoint right_checkpoint →
                MoveDownContext (ChoiceCheckpointD False right_checkpoint (right >>= VisitorT . k)) left_checkpoint (left >>= VisitorT . k)
            _ → throw CacheStepAtChoicePoint
        )
-- @-others
-- @-leo
