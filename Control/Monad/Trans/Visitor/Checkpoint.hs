-- @+leo-ver=5-thin
-- @+node:gcross.20110923120247.1191: * @file Checkpoint.hs
-- @@language haskell

-- @+<< Language extensions >>
-- @+node:gcross.20110923120247.1192: ** << Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
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
import Data.Maybe (mapMaybe)
import Data.Monoid (Monoid(..))
import Data.Monoid.Unicode
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
    CacheCheckpoint ByteString VisitorCheckpoint
  | ChoiceCheckpoint VisitorCheckpoint VisitorCheckpoint
  | Explored
  | Unexplored
  deriving (Eq,Read,Show)
-- @+node:gcross.20111020151748.1287: *3* VisitorCheckpointCursor
type VisitorCheckpointCursor = Seq VisitorCheckpointDifferential
-- @+node:gcross.20111020151748.1286: *3* VisitorCheckpointDifferential
data VisitorCheckpointDifferential =
    CacheCheckpointD ByteString
  | ChoiceCheckpointD Branch VisitorCheckpoint
  deriving (Eq,Read,Show)
-- @+node:gcross.20111223183218.1441: *3* VisitorStatusUpdate
data VisitorStatusUpdate α = VisitorStatusUpdate
    {   visitorStatusCheckpoint :: VisitorCheckpoint
    ,   visitorStatusNewResults :: α
    } deriving (Eq,Show)
-- @+node:gcross.20110923164140.1178: *3* VisitorTContext
type VisitorTContext m α = Seq (VisitorTContextStep m α)
type VisitorContext α = VisitorTContext Identity α
-- @+node:gcross.20110923120247.1196: *3* VisitorTContextStep
data VisitorTContextStep m α =
    BranchContextStep Branch
  | CacheContextStep ByteString
  | LeftChoiceContextStep VisitorCheckpoint (VisitorT m α)

type VisitorContextStep = VisitorTContextStep Identity

instance Show (VisitorTContextStep m α) where
    show (BranchContextStep branch) = "BranchContextStep{" ++ show branch ++ "}"
    show (CacheContextStep c) = "CacheContextStep[" ++ show c ++ "]"
    show (LeftChoiceContextStep checkpoint _) = "LeftChoiceContextStep(" ++ show checkpoint ++ ")"
-- @+node:gcross.20110923164140.1238: *3* VisitorTContextUpdate
type VisitorTContextUpdate m α =
    VisitorTContext m α →
    Maybe (VisitorTContext m α, VisitorCheckpoint, VisitorT m α)

type VisitorContextUpdate α = VisitorTContextUpdate Identity α
-- @+node:gcross.20111019113757.1408: *3* VisitorTResultFetcher
newtype VisitorTResultFetcher m α = VisitorTResultFetcher
    {   fetchVisitorTResult :: m (Maybe (α, VisitorCheckpoint, VisitorTResultFetcher m α))
    }

type VisitorResultFetcher = VisitorTResultFetcher Identity
-- @+node:gcross.20111117140347.1417: ** Exceptions
-- @+node:gcross.20111117140347.1418: *3* InconsistentCheckpoints
data InconsistentCheckpoints = InconsistentCheckpoints VisitorCheckpoint VisitorCheckpoint deriving (Eq,Show,Typeable)

instance Exception InconsistentCheckpoints
-- @+node:gcross.20111117140347.1415: ** Instances
-- @+node:gcross.20111117140347.1416: *3* Monoid VisitorCheckpoint
instance Monoid VisitorCheckpoint where
    mempty = Unexplored
    Explored `mappend` _ = Explored
    _ `mappend` Explored = Explored
    Unexplored `mappend` x = x
    x `mappend` Unexplored = x
    (ChoiceCheckpoint lx rx) `mappend` (ChoiceCheckpoint ly ry) = mergeCheckpointRoot (ChoiceCheckpoint (lx ⊕ ly) (rx ⊕ ry))
    (CacheCheckpoint cx x) `mappend` (CacheCheckpoint cy y)
      | cx == cy = mergeCheckpointRoot (CacheCheckpoint cx (x ⊕ y))
    mappend x y = throw (InconsistentCheckpoints x y)
-- @+node:gcross.20111228145321.1838: *3* Monoid VisitorStatusUpdate
instance Monoid α ⇒ Monoid (VisitorStatusUpdate α) where
    mempty = VisitorStatusUpdate mempty mempty
    VisitorStatusUpdate a1 b1 `mappend` VisitorStatusUpdate a2 b2 = VisitorStatusUpdate (a1 `mappend` a2) (b1 `mappend` b2)
-- @+node:gcross.20110923164140.1179: ** Functions
-- @+node:gcross.20110923164140.1182: *3* checkpointFromContext
checkpointFromContext :: VisitorTContext m α → VisitorCheckpoint → VisitorCheckpoint
checkpointFromContext = checkpointFromSequence $
    \step → case step of
        BranchContextStep LeftBranch → flip ChoiceCheckpoint Explored
        BranchContextStep RightBranch → ChoiceCheckpoint Explored
        CacheContextStep cache → CacheCheckpoint cache
        LeftChoiceContextStep right_checkpoint _ → flip ChoiceCheckpoint right_checkpoint
-- @+node:gcross.20111020182554.1265: *3* checkpointFromCursor
checkpointFromCursor :: VisitorCheckpointCursor → VisitorCheckpoint → VisitorCheckpoint
checkpointFromCursor = checkpointFromSequence $
    \step → case step of
        CacheCheckpointD cache → CacheCheckpoint cache
        ChoiceCheckpointD LeftBranch right_checkpoint → flip ChoiceCheckpoint right_checkpoint
        ChoiceCheckpointD RightBranch left_checkpoint → ChoiceCheckpoint left_checkpoint
-- @+node:gcross.20111117140347.1412: *3* checkpointFromInitialPath
checkpointFromInitialPath :: VisitorPath → VisitorCheckpoint → VisitorCheckpoint
checkpointFromInitialPath = checkpointFromSequence $
    \step → case step of
        CacheStep c → CacheCheckpoint c
        ChoiceStep LeftBranch → flip ChoiceCheckpoint Unexplored
        ChoiceStep RightBranch → ChoiceCheckpoint Unexplored
-- @+node:gcross.20111020182554.1267: *3* checkpointFromSequence
checkpointFromSequence ::
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
-- @+node:gcross.20111029212714.1373: *3* checkpointFromUnexploredPath
checkpointFromUnexploredPath :: VisitorPath → VisitorCheckpoint
checkpointFromUnexploredPath (viewl → EmptyL) = Unexplored
checkpointFromUnexploredPath (viewl → step :< rest_path) =
    case step of
        CacheStep c → CacheCheckpoint c
        ChoiceStep LeftBranch → flip ChoiceCheckpoint Explored
        ChoiceStep RightBranch → ChoiceCheckpoint Explored
    $
    checkpointFromUnexploredPath rest_path
-- @+node:gcross.20111117140347.1438: *3* gatherResults
gatherResults ::
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
-- @+node:gcross.20111117140347.1388: *3* mergeAllCheckpointNodes
mergeAllCheckpointNodes :: VisitorCheckpoint → VisitorCheckpoint
mergeAllCheckpointNodes (ChoiceCheckpoint left right) = mergeCheckpointRoot (ChoiceCheckpoint (mergeAllCheckpointNodes left) (mergeAllCheckpointNodes right))
mergeAllCheckpointNodes (CacheCheckpoint cache checkpoint) = mergeCheckpointRoot (CacheCheckpoint cache (mergeAllCheckpointNodes checkpoint))
mergeAllCheckpointNodes checkpoint = checkpoint
-- @+node:gcross.20111020182554.1266: *3* mergeCheckpointRoot
mergeCheckpointRoot :: VisitorCheckpoint → VisitorCheckpoint
mergeCheckpointRoot (ChoiceCheckpoint Unexplored Unexplored) = Unexplored
mergeCheckpointRoot (ChoiceCheckpoint Explored Explored) = Explored
mergeCheckpointRoot (CacheCheckpoint _ Explored) = Explored
mergeCheckpointRoot checkpoint = checkpoint
-- @+node:gcross.20111020182554.1281: *3* moveDownContext
moveDownContext ::
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
-- @+node:gcross.20110923164140.1239: *3* moveUpContext
moveUpContext :: VisitorTContextUpdate m α
moveUpContext (viewr → EmptyR) = Nothing
moveUpContext (viewr → rest_context :> BranchContextStep _) = moveUpContext rest_context
moveUpContext (viewr → rest_context :> CacheContextStep _) = moveUpContext rest_context
moveUpContext (viewr → rest_context :> LeftChoiceContextStep right_checkpoint right_visitor) =
    Just (rest_context |> BranchContextStep RightBranch
         ,right_checkpoint
         ,right_visitor
         )
-- @+node:gcross.20110923164140.1265: *3* pathFromContext
pathFromContext :: VisitorTContext m α → VisitorPath
pathFromContext = fmap pathStepFromContextStep
-- @+node:gcross.20111020182554.1273: *3* pathFromCursor
pathFromCursor :: VisitorCheckpointCursor → VisitorPath
pathFromCursor = fmap pathStepFromCursorDifferential
-- @+node:gcross.20110923164140.1266: *3* pathStepFromContextStep
pathStepFromContextStep :: VisitorTContextStep m α → VisitorStep
pathStepFromContextStep (BranchContextStep active_branch) = ChoiceStep active_branch
pathStepFromContextStep (CacheContextStep cache) = CacheStep cache
pathStepFromContextStep (LeftChoiceContextStep _ _) = ChoiceStep LeftBranch
-- @+node:gcross.20111020182554.1271: *3* pathStepFromCursorDifferential
pathStepFromCursorDifferential :: VisitorCheckpointDifferential → VisitorStep
pathStepFromCursorDifferential (CacheCheckpointD cache) = CacheStep cache
pathStepFromCursorDifferential (ChoiceCheckpointD active_branch _) = ChoiceStep active_branch
-- @+node:gcross.20111029212714.1368: *3* runVisitorThroughCheckpoint
runVisitorThroughCheckpoint ::
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
-- @+node:gcross.20120102210156.1898: *3* runVisitorThroughCheckpointAndGatherResults
runVisitorThroughCheckpointAndGatherResults ::
    Monoid α ⇒
    VisitorCheckpoint →
    Visitor α →
    α
runVisitorThroughCheckpointAndGatherResults = (fst . last) .* runVisitorThroughCheckpoint
-- @+node:gcross.20111029212714.1366: *3* runVisitorTThroughCheckpoint
runVisitorTThroughCheckpoint ::
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
-- @+node:gcross.20120102210156.1896: *3* runVisitorTThroughCheckpointAndGatherResults
runVisitorTThroughCheckpointAndGatherResults ::
    (Functor m, Monad m, Monoid α) ⇒
    VisitorCheckpoint →
    VisitorT m α →
    m α
runVisitorTThroughCheckpointAndGatherResults = gatherResults .* runVisitorTThroughCheckpoint
-- @+node:gcross.20111028170027.1294: *3* runVisitorTWithCheckpoints
runVisitorTTWithCheckpoints ::
    (Functor m, Monad m, Monoid α) ⇒
    VisitorT m α →
    VisitorTResultFetcher m α
runVisitorTTWithCheckpoints = runVisitorTThroughCheckpoint Unexplored
-- @+node:gcross.20111028170027.1292: *3* runVisitorWithCheckpoints
runVisitorWithCheckpoints ::
    Monoid α ⇒
    Visitor α →
    [(α,VisitorCheckpoint)]
runVisitorWithCheckpoints = runVisitorThroughCheckpoint Unexplored
-- @+node:gcross.20110923164140.1242: *3* stepVisitorThroughCheckpoint
stepVisitorThroughCheckpoint ::
    VisitorContext α →
    VisitorCheckpoint →
    Visitor α →
    (Maybe α,Maybe (VisitorContext α, VisitorCheckpoint, Visitor α))
stepVisitorThroughCheckpoint context checkpoint = runIdentity . stepVisitorTThroughCheckpoint context checkpoint
-- @+node:gcross.20110923164140.1186: *3* stepVisitorTThroughCheckpoint
stepVisitorTThroughCheckpoint ::
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
-- @-others
-- @-leo
