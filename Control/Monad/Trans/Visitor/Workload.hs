-- @+leo-ver=5-thin
-- @+node:gcross.20111029192420.1349: * @file Control/Monad/Trans/Visitor/Workload.hs
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
import Data.Maybe (catMaybes)

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
-- @+node:gcross.20111029192420.1352: ** Functions
-- @+node:gcross.20111029192420.1356: *3* runVisitorTThroughWorkload
runVisitorTThroughWorkload ::
    (Functor m, Monad m) ⇒
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
        runVisitorTThroughCheckpointWithStartingLabel
            (labelFromPath visitorWorkloadPath)
            visitorWorkloadCheckpoint
    )
    .
    walkVisitorTDownPath visitorWorkloadPath
-- @+node:gcross.20111029212714.1370: *3* runVisitorThroughWorkload
runVisitorThroughWorkload ::
    VisitorWorkload →
    Visitor α →
    [(Maybe (VisitorSolution α),VisitorCheckpoint)]
runVisitorThroughWorkload VisitorWorkload{..} =
    runVisitorThroughCheckpointWithStartingLabel
        (labelFromPath visitorWorkloadPath)
        visitorWorkloadCheckpoint
    .
    walkVisitorDownPath visitorWorkloadPath
-- @+node:gcross.20111116214909.1373: *3* runVisitorThroughWorkloadAndReturnResults
runVisitorThroughWorkloadAndReturnResults ::
    VisitorWorkload →
    Visitor α →
    [VisitorSolution α]
runVisitorThroughWorkloadAndReturnResults =
    (catMaybes . map fst)
    .*
    runVisitorThroughWorkload
-- @-others
-- @-leo
