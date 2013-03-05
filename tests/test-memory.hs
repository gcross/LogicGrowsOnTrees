-- Language extensions {{{
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

-- Imports {{{
import Control.Exception (evaluate)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO,liftIO)

import Data.Functor
import Data.Monoid
import Data.Word

import System.Log.Logger (Priority(..),updateGlobalLogger,rootLoggerName,setLevel)
import System.Mem

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Text.Printf

import Control.Visitor
import Control.Visitor.Checkpoint
import Control.Visitor.Examples.RoseTree
import Control.Visitor.Main (RunOutcome(..),TerminationReason(..))
import qualified Control.Visitor.Parallel.Threads as Threads
import Control.Visitor.Parallel.Workgroup (changeNumberOfWorkers)
import Control.Visitor.Utils.WordSum
import Control.Visitor.Visitors.RoseTree
import qualified Control.Visitor.Worker as Worker
-- }}}

-- Functions {{{

runVisitorThreads :: (Monoid α, Show α) ⇒ IO Int → Visitor α → IO α -- {{{
runVisitorThreads getNumberOfThreads visitor =
    Threads.runVisitor visitor (void . changeNumberOfWorkers . const . liftIO $ getNumberOfThreads)
    >>=
    \run_outcome → case runTerminationReason run_outcome of
        Completed result → evaluate result
        _ → error $ "thread workgroup terminated unexpectedly: " ++ show (runTerminationReason run_outcome) 
-- }}}

runVisitorWorker :: (Monoid α, Show α) ⇒ Visitor α → IO α -- {{{
runVisitorWorker visitor =
    Worker.runVisitor visitor
    >>=
    \termination_reason → case termination_reason of
        Worker.WorkerFinished (Progress Explored result) → evaluate result
        _ → error $ "worker terminated unexpectedly: " ++ show termination_reason 
-- }}}

testMemory :: String → Assertion → Test.Framework.Test -- {{{
testMemory name action = testCase name $ performGC >> action >> performGC
-- }}}

testTrivialNodeSum :: (Visitor WordSum → IO WordSum) → Word → Word → Test.Framework.Test
testTrivialNodeSum runVisitor arity depth = -- {{{
    testCase (printf "arity = %i, depth = %i" arity depth) $ do
        getWordSum <$> (runVisitor . sumOverAllNodes $ generateTrivialTree arity depth)
        >>=
        (@?= computeCorrectTrivialTreeSumOverNodes arity depth)
-- }}}

-- }}}

main = do
    -- updateGlobalLogger rootLoggerName (setLevel DEBUG)
    defaultMain tests

tests = -- {{{
    [testGroup "using runVisitor" -- {{{
        [testGroup "sum trivial tree nodes" -- {{{
            [testTrivialNodeSum (evaluate . runVisitor) 2 10
            ,testTrivialNodeSum (evaluate . runVisitor) 2 20
            ]
         -- }}}
        ]
     -- }}}
    ,testGroup "using runVisitorThroughCheckpoint" -- {{{
        [testGroup "sum trivial tree nodes" -- {{{
            [testTrivialNodeSum (evaluate . runVisitorThroughCheckpoint Unexplored) 2 10
            ,testTrivialNodeSum (evaluate . runVisitorThroughCheckpoint Unexplored) 2 20
            ]
         -- }}}
        ]
    ,testGroup "using worker" -- {{{
        [testGroup "sum trivial tree nodes" -- {{{
            [testTrivialNodeSum runVisitorWorker 2 10
            ,testTrivialNodeSum runVisitorWorker 2 20
            ]
         -- }}}
        ]
     -- }}}
    ,testGroup "using one thread" -- {{{
        [testGroup "sum trivial tree nodes" -- {{{
            [testTrivialNodeSum (runVisitorThreads (return 1)) 2 5
            ,testTrivialNodeSum (runVisitorThreads (return 1)) 2 10
            ,testTrivialNodeSum (runVisitorThreads (return 1)) 2 20
            ]
         -- }}}
        ]
     -- }}}
    ]
-- }}}
