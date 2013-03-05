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

testAll :: (Visitor WordSum,Word) → [Test.Framework.Test] -- {{
testAll (visitor,correct_result) =
    [testCase "plain visitor" $
        getWordSum (runVisitor visitor) @?= correct_result
    ,testCase "checkpointing visitor" $
        getWordSum (runVisitorThroughCheckpoint Unexplored visitor) @?= correct_result
    ,testCase "worker" $
        (getWordSum  <$> runVisitorWorker visitor) >>= (@?= correct_result)
    ,testCase "one thread" $
        (getWordSum  <$> runVisitorThreads (return 1) visitor) >>= (@?= correct_result)
    ]
-- }}}

main = do
    -- updateGlobalLogger rootLoggerName (setLevel DEBUG)
    defaultMain tests

tests = -- {{{
    [testGroup "sum trivial tree nodes" $ -- {{{
        let constructVisitorAndCorrectResult arity depth =
                (sumOverAllNodes $ generateTrivialTree arity depth
                ,computeCorrectTrivialTreeSumOverNodes arity depth
                )
        in
        [testGroup "arity = 2, depth =  5" . testAll $
            constructVisitorAndCorrectResult 2  5
        ,testGroup "arity = 2, depth = 10" . testAll $
            constructVisitorAndCorrectResult 2 10
        ,testGroup "arity = 2, depth = 15" . testAll $
            constructVisitorAndCorrectResult 2 15
        ,testGroup "arity = 2, depth = 25" . testAll $
            constructVisitorAndCorrectResult 2 25
        ]
     -- }}}
    ]
-- }}}
