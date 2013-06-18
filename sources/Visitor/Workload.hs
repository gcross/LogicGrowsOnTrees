-- Language extensions {{{
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Visitor.Workload where

-- Imports {{{
import Control.Monad ((>=>),join,liftM)
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
    TreeGenerator α →
    α
visitTreeThroughWorkload Workload{..} =
    visitTreeStartingFromCheckpoint workloadCheckpoint
    .
    sendTreeGeneratorDownPath workloadPath
-- }}}

visitTreeTThroughWorkload :: -- {{{
    (Monad m, Monoid α) ⇒
    Workload →
    TreeGeneratorT m α →
    m α
visitTreeTThroughWorkload Workload{..} =
    sendTreeGeneratorTDownPath workloadPath
    >=>
    visitTreeTStartingFromCheckpoint workloadCheckpoint
-- }}}

visitTreeUntilFirstThroughWorkload :: -- {{{
    Workload →
    TreeGenerator α →
    Maybe α
visitTreeUntilFirstThroughWorkload Workload{..} =
    visitTreeUntilFirstStartingFromCheckpoint workloadCheckpoint
    .
    sendTreeGeneratorDownPath workloadPath
-- }}}

visitTreeTUntilFirstThroughWorkload :: -- {{{
    Monad m ⇒
    Workload →
    TreeGeneratorT m α →
    m (Maybe α)
visitTreeTUntilFirstThroughWorkload Workload{..} =
    sendTreeGeneratorTDownPath workloadPath
    >=>
    visitTreeTUntilFirstStartingFromCheckpoint workloadCheckpoint
-- }}}

visitTreeUntilFoundThroughWorkload :: -- {{{
    Monoid α ⇒
    (α → Maybe β) →
    Workload →
    TreeGenerator α →
    Either α β
visitTreeUntilFoundThroughWorkload condition Workload{..} =
    visitTreeUntilFoundStartingFromCheckpoint condition workloadCheckpoint
    .
    sendTreeGeneratorDownPath workloadPath
-- }}}

visitTreeTUntilFoundThroughWorkload :: -- {{{
    (Monoid α, Monad m) ⇒
    (α → Maybe β) →
    Workload →
    TreeGeneratorT m α →
    m (Either α β)
visitTreeTUntilFoundThroughWorkload condition Workload{..} =
    sendTreeGeneratorTDownPath workloadPath
    >=>
    visitTreeTUntilFoundStartingFromCheckpoint condition workloadCheckpoint
-- }}}

workloadDepth :: Workload → Int -- {{{
workloadDepth = Seq.length . workloadPath -- }}}

-- }}}
