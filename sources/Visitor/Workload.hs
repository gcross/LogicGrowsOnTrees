-- Language extensions {{{
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Visitor.Workload where

-- Imports {{{
import Control.Monad (join,liftM)
import Data.Composition ((.*),(.**))
import Data.Derive.Serialize
import Data.DeriveTH
import Data.Function (on)
import Data.Monoid (Monoid(..))
import qualified Data.Sequence as Seq
import Data.Serialize

import Visitor
import Visitor.Checkpoint
import Visitor.Path
-- }}}

-- Types {{{

data Workload = Workload -- {{{
    {   workloadPath :: Path
    ,   workloadCheckpoint :: Checkpoint
    } deriving (Eq,Show)
$( derive makeSerialize ''Workload )
-- }}}

-- }}}

-- Instances {{{
instance Ord Workload where
    x `compare` y =
        case (compare `on` workloadDepth) x y of
            EQ → case (compare `on` workloadPath) x y of
                EQ → (compare `on` workloadCheckpoint) x y
                c → c
            c → c
-- }}}

-- Values {{{

entire_workload :: Workload
entire_workload = Workload Seq.empty Unexplored

-- }}}

-- Functions {{{

visitTreeThroughWorkload :: -- {{{
    Monoid α ⇒
    Workload →
    TreeBuilder α →
    α
visitTreeThroughWorkload =
    (fst . last)
    .*
    walkThroughTreeThroughWorkload
-- }}}

visitTreeTThroughWorkload :: -- {{{
    (Monad m, Monoid α) ⇒
    Workload →
    TreeBuilderT m α →
    m α
visitTreeTThroughWorkload = gatherResults .* walkThroughTreeTThroughWorkload
-- }}}

visitTreeUntilFirstThroughWorkload :: -- {{{
    Workload →
    TreeBuilder α →
    Maybe α
visitTreeUntilFirstThroughWorkload =
    fetchFirstResult
    .*
    walkThroughTreeUntilFirstThroughWorkload
-- }}}

visitTreeTUntilFirstThroughWorkload :: -- {{{
    Monad m ⇒
    Workload →
    TreeBuilderT m α →
    m (Maybe α)
visitTreeTUntilFirstThroughWorkload =
    fetchFirstResultT
    .*
    walkThroughTreeTUntilFirstThroughWorkload
-- }}}

visitTreeUntilFoundThroughWorkload :: -- {{{
    Monoid α ⇒
    (α → Maybe β) →
    Workload →
    TreeBuilder α →
    Either α β
visitTreeUntilFoundThroughWorkload =
    fetchFoundResult
    .**
    walkThroughTreeUntilFoundThroughWorkload
-- }}}

visitTreeTUntilFoundThroughWorkload :: -- {{{
    (Monoid α, Monad m) ⇒
    (α → Maybe β) →
    Workload →
    TreeBuilderT m α →
    m (Either α β)
visitTreeTUntilFoundThroughWorkload =
    fetchFoundResultT
    .**
    walkThroughTreeTUntilFoundThroughWorkload
-- }}}

walkThroughTreeThroughWorkload :: -- {{{
    Monoid α ⇒
    Workload →
    TreeBuilder α →
    [(α,Checkpoint)]
walkThroughTreeThroughWorkload Workload{..} =
    walkThroughTreeStartingFromCheckpoint workloadCheckpoint
    .
    sendVisitorDownPath workloadPath
-- }}}

walkThroughTreeTThroughWorkload :: -- {{{
    (Monad m, Monoid α) ⇒
    Workload →
    TreeBuilderT m α →
    ResultFetcher m α
walkThroughTreeTThroughWorkload Workload{..} =
    ResultFetcher
    .
    join
    .
    liftM (
        fetchResult
        .
        walkThroughTreeTStartingFromCheckpoint workloadCheckpoint
    )
    .
    sendVisitorTDownPath workloadPath
-- }}}

walkThroughTreeUntilFirstThroughWorkload :: -- {{{
    Workload →
    TreeBuilder α →
    FirstResultFetcher α
walkThroughTreeUntilFirstThroughWorkload Workload{..} =
    walkThroughTreeUntilFirstStartingFromCheckpoint workloadCheckpoint
    .
    sendVisitorDownPath workloadPath
-- }}}

walkThroughTreeTUntilFirstThroughWorkload :: -- {{{
    Monad m ⇒
    Workload →
    TreeBuilderT m α →
    FirstResultFetcherT m α
walkThroughTreeTUntilFirstThroughWorkload Workload{..} =
    FirstResultFetcherT
    .
    join
    .
    liftM (
        firstResultFetcher
        .
        walkThroughTreeTUntilFirstStartingFromCheckpoint workloadCheckpoint
    )
    .
    sendVisitorTDownPath workloadPath
-- }}}

walkThroughTreeUntilFoundThroughWorkload :: -- {{{
    Monoid α ⇒
    (α → Maybe β) →
    Workload →
    TreeBuilder α →
    FoundResultFetcher α β
walkThroughTreeUntilFoundThroughWorkload f Workload{..} =
    walkThroughTreeUntilFoundStartingFromCheckpoint f workloadCheckpoint
    .
    sendVisitorDownPath workloadPath
-- }}}

walkThroughTreeTUntilFoundThroughWorkload :: -- {{{
    (Monoid α, Monad m) ⇒
    (α → Maybe β) →
    Workload →
    TreeBuilderT m α →
    FoundResultFetcherT m α β
walkThroughTreeTUntilFoundThroughWorkload f Workload{..} =
    FoundResultFetcherT
    .
    join
    .
    liftM (
        foundResultFetcher
        .
        walkThroughTreeTUntilFoundStartingFromCheckpoint f workloadCheckpoint
    )
    .
    sendVisitorTDownPath workloadPath
-- }}}

workloadDepth :: Workload → Int -- {{{
workloadDepth = Seq.length . workloadPath -- }}}

-- }}}
