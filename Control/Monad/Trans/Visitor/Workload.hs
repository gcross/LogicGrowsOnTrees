-- @+leo-ver=5-thin
-- @+node:gcross.20111029192420.1349: * @file Workload.hs
-- @@language haskell

-- @+<< Language extensions >>
-- @+node:gcross.20111029192420.1350: ** << Language extensions >>
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-<< Language extensions >>

module Control.Monad.Trans.Visitor.Workload where

-- @+<< Import needed modules >>
-- @+node:gcross.20111029192420.1351: ** << Import needed modules >>
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
-- @-<< Import needed modules >>

-- @+others
-- @+node:gcross.20111029192420.1355: ** Types
-- @+node:gcross.20111029192420.1354: *3* VisitorWorkload
data VisitorWorkload = VisitorWorkload
    {   visitorWorkloadPath :: VisitorPath
    ,   visitorWorkloadCheckpoint :: VisitorCheckpoint
    } deriving (Eq,Show)
-- @+node:gcross.20120101231106.1863: ** Instances
-- @+node:gcross.20120101231106.1864: *3* Ord VisitorWorkload
instance Ord VisitorWorkload where
    compare = compare `on` (Seq.length . visitorWorkloadPath)
-- @+node:gcross.20111117140347.1386: ** Constants
-- @+node:gcross.20111117140347.1385: *3* entire_workload
entire_workload = VisitorWorkload Seq.empty Unexplored
-- @+node:gcross.20111029192420.1352: ** Functions
-- @+node:gcross.20111029192420.1356: *3* runVisitorTThroughWorkload
runVisitorTThroughWorkload ::
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
-- @+node:gcross.20111117140347.1437: *3* runVisitorTThroughWorkloadAndGatherResults
runVisitorTThroughWorkloadAndGatherResults ::
    (Functor m, Monad m, Monoid α) ⇒
    VisitorWorkload →
    VisitorT m α →
    m α
runVisitorTThroughWorkloadAndGatherResults = gatherResults .* runVisitorTThroughWorkload
-- @+node:gcross.20111029212714.1370: *3* runVisitorThroughWorkload
runVisitorThroughWorkload ::
    Monoid α ⇒
    VisitorWorkload →
    Visitor α →
    [(α,VisitorCheckpoint)]
runVisitorThroughWorkload VisitorWorkload{..} =
    runVisitorThroughCheckpoint visitorWorkloadCheckpoint
    .
    walkVisitorDownPath visitorWorkloadPath
-- @+node:gcross.20111116214909.1373: *3* runVisitorThroughWorkloadAndGatherResults
runVisitorThroughWorkloadAndGatherResults ::
    Monoid α ⇒
    VisitorWorkload →
    Visitor α →
    α
runVisitorThroughWorkloadAndGatherResults =
    (mconcat . map fst)
    .*
    runVisitorThroughWorkload
-- @-others
-- @-leo
