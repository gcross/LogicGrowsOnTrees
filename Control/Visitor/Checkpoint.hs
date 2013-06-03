-- Language extensions {{{
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- }}}

module Control.Visitor.Checkpoint where

-- Imports {{{
import Control.Arrow (second)
import Control.Exception (Exception(),throw)
import Control.Monad ((>=>),join,liftM)
import Control.Monad.Operational (ProgramViewT(..),viewT)
import Control.Monad.Trans.Class (MonadTrans(..))

import Data.ByteString (ByteString)
import Data.Composition
import Data.Derive.Monoid
import Data.Derive.Serialize
import Data.DeriveTH
import Data.Either.Unwrap (mapLeft)
import Data.Functor.Identity (Identity,runIdentity)
import Data.Maybe (isJust,mapMaybe)
import Data.Monoid ((<>),First(..),Monoid(..))
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

data FirstResultFetcher α = -- {{{
    DoneFetchingFirst (Maybe α)
  | StillFetchingFirst Checkpoint (FirstResultFetcher α)
-- }}}

newtype FirstResultFetcherT m α = FirstResultFetcherT -- {{{
    {   firstResultFetcher :: m (Either (Checkpoint, FirstResultFetcherT m α) (Maybe α))
    }
-- }}}

data FoundResultFetcher α β = -- {{{
    DoneFetchingFound (Either α β)
  | StillFetchingFound Checkpoint (FoundResultFetcher α β)
-- }}}

newtype FoundResultFetcherT m α β = FoundResultFetcherT -- {{{
    {   foundResultFetcher :: m (Either (Checkpoint, FoundResultFetcherT m α β) (Either α β))
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

checkpointFromVisitorState :: VisitorTState m α → Checkpoint -- {{{
checkpointFromVisitorState VisitorTState{..} =
    checkpointFromContext visitorStateContext visitorStateCheckpoint
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

fetchFirstResult :: FirstResultFetcher α → Maybe α -- {{{
fetchFirstResult (DoneFetchingFirst maybe_result) = maybe_result
fetchFirstResult (StillFetchingFirst _ next_fetcher) = fetchFirstResult next_fetcher
-- }}}

fetchFirstResultT :: Monad m ⇒ FirstResultFetcherT m α → m (Maybe α) -- {{{
fetchFirstResultT = firstResultFetcher >=> either (fetchFirstResultT . snd) return
-- }}}

fetchFoundResult :: FoundResultFetcher α β → Either α β -- {{{
fetchFoundResult (DoneFetchingFound maybe_result) = maybe_result
fetchFoundResult (StillFetchingFound _ next_fetcher) = fetchFoundResult next_fetcher
-- }}}

fetchFoundResultT :: Monad m ⇒ FoundResultFetcherT m α β → m (Either α β) -- {{{
fetchFoundResultT = foundResultFetcher >=> either (fetchFoundResultT . snd) return
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

runVisitorUntilFirstThroughCheckpoint :: -- {{{
    Checkpoint →
    Visitor α →
    Maybe α
runVisitorUntilFirstThroughCheckpoint = runIdentity .* runVisitorTUntilFirstThroughCheckpoint
-- }}}

runVisitorTUntilFirstThroughCheckpoint :: -- {{{
    Monad m ⇒
    Checkpoint →
    VisitorT m α →
    m (Maybe α)
runVisitorTUntilFirstThroughCheckpoint = go .* initialVisitorState
  where
    go = stepVisitorTThroughCheckpoint
         >=>
         \(maybe_solution,maybe_new_visitor_state) →
            case maybe_solution of
                Just _ → return maybe_solution
                Nothing → maybe (return Nothing) go maybe_new_visitor_state
{-# INLINE runVisitorTUntilFirstThroughCheckpoint #-}
-- }}}

runVisitorUntilFoundThroughCheckpoint :: -- {{{
    Monoid α ⇒
    (α → Maybe β) →
    Checkpoint →
    Visitor α →
    Either α β
runVisitorUntilFoundThroughCheckpoint = runIdentity .** runVisitorTUntilFoundThroughCheckpoint
-- }}}

runVisitorTUntilFoundThroughCheckpoint :: -- {{{
    (Monad m, Monoid α) ⇒
    (α → Maybe β) →
    Checkpoint →
    VisitorT m α →
    m (Either α β)
runVisitorTUntilFoundThroughCheckpoint f = go mempty .* initialVisitorState
  where
    go accum =
        stepVisitorTThroughCheckpoint
        >=>
        \(maybe_solution,maybe_new_visitor_state) →
            case maybe_solution of
                Nothing → maybe (return (Left accum)) (go accum) maybe_new_visitor_state
                Just solution →
                    let new_accum = accum <> solution
                    in case f new_accum of
                        Nothing → maybe (return (Left new_accum)) (go new_accum) maybe_new_visitor_state
                        Just result → return (Right result)
{-# INLINE runVisitorTUntilFoundThroughCheckpoint #-}
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

walkVisitorUntilFirst :: -- {{{
    Visitor α →
    FirstResultFetcher α
walkVisitorUntilFirst = walkVisitorUntilFirstThroughCheckpoint Unexplored
-- }}}

walkVisitorTUntilFirst :: -- {{{
    Monad m ⇒
    VisitorT m α →
    FirstResultFetcherT m α
walkVisitorTUntilFirst = walkVisitorTUntilFirstThroughCheckpoint Unexplored
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
                Just new_state →
                    (new_accum
                    ,checkpointFromVisitorState new_state
                    ,go new_accum new_state
                    )
{-# INLINE walkVisitorTThroughCheckpoint #-}
-- }}}

walkVisitorUntilFirstThroughCheckpoint :: -- {{{
    Checkpoint →
    Visitor α →
    FirstResultFetcher α
walkVisitorUntilFirstThroughCheckpoint = go .* initialVisitorState
  where
    go visitor_state
      | isJust maybe_solution = DoneFetchingFirst maybe_solution
      | otherwise =
         case maybe_new_state of
            Nothing → DoneFetchingFirst Nothing
            Just new_state →
                StillFetchingFirst
                    (checkpointFromVisitorState new_state)
                    (go new_state)
      where
        (maybe_solution,maybe_new_state) = stepVisitorThroughCheckpoint visitor_state
-- }}}

walkVisitorTUntilFirstThroughCheckpoint :: -- {{{
    Monad m ⇒
    Checkpoint →
    VisitorT m α →
    FirstResultFetcherT m α
walkVisitorTUntilFirstThroughCheckpoint = go .* initialVisitorState
  where
    go visitor_state = FirstResultFetcherT $
        stepVisitorTThroughCheckpoint visitor_state
        >>=
        \(maybe_solution,maybe_new_state) → return $
            case maybe_solution of
                Just _ → Right maybe_solution
                Nothing →
                    case maybe_new_state of
                        Nothing → Right Nothing
                        Just new_state →
                            Left
                            $
                            (checkpointFromVisitorState new_state
                            ,go new_state
                            )
{-# INLINE walkVisitorTUntilFirstThroughCheckpoint #-}
-- }}}

walkVisitorUntilFoundThroughCheckpoint :: -- {{{
    Monoid α ⇒
    (α → Maybe β) →
    Checkpoint →
    Visitor α →
    FoundResultFetcher α β
walkVisitorUntilFoundThroughCheckpoint f = go mempty .* initialVisitorState
  where
    go result visitor_state =
        case maybe_solution of
            Just solution →
                let new_result = result <> solution
                in case f new_result of
                    Just x → DoneFetchingFound (Right x)
                    Nothing → continueWith new_result
            Nothing → continueWith result
      where
        (maybe_solution,maybe_new_state) = stepVisitorThroughCheckpoint visitor_state

        continueWith current_result =
            case maybe_new_state of
                Nothing → DoneFetchingFound (Left current_result)
                Just new_state →
                    StillFetchingFound
                        (checkpointFromVisitorState new_state)
                        (go current_result new_state)
-- }}}

walkVisitorTUntilFoundThroughCheckpoint :: -- {{{
    (Monoid α, Monad m) ⇒
    (α → Maybe β) →
    Checkpoint →
    VisitorT m α →
    FoundResultFetcherT m α β
walkVisitorTUntilFoundThroughCheckpoint f = go mempty .* initialVisitorState
  where
    go result visitor_state = FoundResultFetcherT $ do
        (maybe_solution,maybe_new_state) ← stepVisitorTThroughCheckpoint visitor_state
        let continueWith current_result =
                case maybe_new_state of
                    Nothing → (Right . Left $ current_result)
                    Just new_state →
                        Left
                        $
                        (checkpointFromVisitorState new_state
                        ,go current_result new_state
                        )
        return $
            case maybe_solution of
                Nothing → continueWith result
                Just solution →
                    let new_result = result <> solution
                    in case f new_result of
                        Just x → (Right . Right $ x)
                        Nothing → continueWith new_result
{-# INLINE walkVisitorTUntilFoundThroughCheckpoint #-}
-- }}}

-- }}}
