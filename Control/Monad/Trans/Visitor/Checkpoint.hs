-- @+leo-ver=5-thin
-- @+node:gcross.20110923120247.1191: * @file Control/Monad/Trans/Visitor/Checkpoint.hs
-- @@language haskell

-- @+<< Language extensions >>
-- @+node:gcross.20110923120247.1192: ** << Language extensions >>
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- @-<< Language extensions >>

module Control.Monad.Trans.Visitor.Checkpoint where

-- @+<< Import needed modules >>
-- @+node:gcross.20110923120247.1193: ** << Import needed modules >>
import Control.Exception (Exception(),throw)
import Control.Monad ((>=>))
import Control.Monad.Operational (ProgramViewT(..),viewT)

import Data.ByteString (ByteString)
import Data.Sequence ((|>),Seq,viewl,ViewL(..))
import qualified Data.Sequence as Seq
import Data.Serialize (Serialize(),decode)
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
-- @+node:gcross.20110923120247.1196: *3* VisitorCheckpointDifferential
data VisitorCheckpointDifferential m α =
    BranchCheckpointD Bool
  | CacheCheckpointD ByteString
  | ChoiceCheckpointD Bool VisitorCheckpoint (VisitorT m α)
-- @+node:gcross.20110923164140.1178: *3* VisitorCheckpointContext
data VisitorCheckpointContext m α = VisitorCheckpointContext (Seq (VisitorCheckpointDifferential m α)) (VisitorT m α)
-- @+node:gcross.20110923164140.1179: ** Functions
-- @+node:gcross.20110923164140.1182: *3* checkpointFromContext
checkpointFromContext :: VisitorCheckpointContext m α → VisitorCheckpoint
checkpointFromContext (VisitorCheckpointContext difference _) = go difference
  where
    go (viewl → EmptyL) = Unexplored
    go (viewl → differential :< rest) =
        let rest_checkpoint = go rest
        in case differential of
            BranchCheckpointD which_explored → BranchCheckpoint which_explored rest_checkpoint
            CacheCheckpointD cache → CacheCheckpoint cache rest_checkpoint
            ChoiceCheckpointD False right_checkpoint _ → ChoiceCheckpoint rest_checkpoint right_checkpoint
            ChoiceCheckpointD True left_checkpoint _ → ChoiceCheckpoint left_checkpoint rest_checkpoint
-- @+node:gcross.20110923164140.1185: *3* enterUnexploredCheckpointContext
enterUnexploredCheckpointContext :: Monad m ⇒ VisitorCheckpoint → VisitorT m α → m (VisitorCheckpointContext m α)
enterUnexploredCheckpointContext = go Seq.empty
  where
    go difference Unexplored = return . VisitorCheckpointContext difference
    go difference context =
        viewT >=> \view → case (view,context) of
            (Return x,_) → throw DeadEnd
            (Null :>>= _,_) → throw DeadEnd
            (IsFirstVisit :>>= k,_) →
                go difference context $ k False
            (Cache mx :>>= k,CacheCheckpoint cache rest_checkpoint) →
                go (difference |> CacheCheckpointD cache) rest_checkpoint $ either error k (decode cache)
            (Cache _ :>>= _,_) → throw ChoiceStepAtCachePoint
            (Choice left _ :>>= k,BranchCheckpoint True left_checkpoint) →
                go (difference |> BranchCheckpointD True) left_checkpoint (left >>= k)
            (Choice _ right :>>= k,BranchCheckpoint False right_checkpoint) →
                go (difference |> BranchCheckpointD False) right_checkpoint (right >>= k)
            (Choice left right :>>= k,ChoiceCheckpoint left_checkpoint right_checkpoint) →
                go (difference |> ChoiceCheckpointD False right_checkpoint (right >>= k)) left_checkpoint (left >>= k)
            (Choice _ _ :>>= _,_) → throw CacheStepAtChoicePoint
-- @-others
-- @-leo
