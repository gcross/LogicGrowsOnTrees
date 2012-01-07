-- Language extensions {{{
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Control.Monad.Trans.Visitor.Workload where

-- Imports {{{
import Control.Monad (join)
import Data.Composition ((.*))
import Data.Function (on)
import Data.Maybe (catMaybes)
import Data.Monoid (Monoid(..))
import qualified Data.Sequence as Seq

import Control.Monad.Trans.Visitor
import Control.Monad.Trans.Visitor.Checkpoint
import Control.Monad.Trans.Visitor.Label
import Control.Monad.Trans.Visitor.Path
-- }}}

-- Types {{{

data VisitorWorkload = VisitorWorkload -- {{{
    {   visitorWorkloadPath :: VisitorPath
    ,   visitorWorkloadCheckpoint :: VisitorCheckpoint
    } deriving (Eq,Show)
-- }}}

-- }}}

-- Instances {{{

instance Ord VisitorWorkload where
    compare = compare `on` (Seq.length . visitorWorkloadPath)

-- }}}

-- Values {{{

entire_workload = VisitorWorkload Seq.empty Unexplored

-- }}}

-- Functions {{{

runVisitorTThroughWorkload :: -- {{{
    (Functor m, Monad m, Monoid α) ⇒
    VisitorWorkload →
    VisitorT m α →
    VisitorTResultFetcher m α
runVisitorTThroughWorkload VisitorWorkload{..} =
    VisitorTResultFetcher
    .
    join
    .
    fmap (
        fetchVisitorTResult
        .
        runVisitorTThroughCheckpoint visitorWorkloadCheckpoint
    )
    .
    walkVisitorTDownPath visitorWorkloadPath
-- }}}

runVisitorTThroughWorkloadAndGatherResults :: -- {{{
    (Functor m, Monad m, Monoid α) ⇒
    VisitorWorkload →
    VisitorT m α →
    m α
runVisitorTThroughWorkloadAndGatherResults = gatherResults .* runVisitorTThroughWorkload
-- }}}

runVisitorThroughWorkload :: -- {{{
    Monoid α ⇒
    VisitorWorkload →
    Visitor α →
    [(α,VisitorCheckpoint)]
runVisitorThroughWorkload VisitorWorkload{..} =
    runVisitorThroughCheckpoint visitorWorkloadCheckpoint
    .
    walkVisitorDownPath visitorWorkloadPath
-- }}}

runVisitorThroughWorkloadAndGatherResults :: -- {{{
    Monoid α ⇒
    VisitorWorkload →
    Visitor α →
    α
runVisitorThroughWorkloadAndGatherResults =
    (mconcat . map fst)
    .*
    runVisitorThroughWorkload
-- }}}

-- }}}
