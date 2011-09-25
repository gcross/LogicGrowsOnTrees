-- @+leo-ver=5-thin
-- @+node:gcross.20101114125204.1259: * @file tests.hs
-- @@language haskell

-- @+<< Language extensions >>
-- @+node:gcross.20101114125204.1281: ** << Language extensions >>
{-# LANGUAGE UnicodeSyntax #-}
-- @-<< Language extensions >>

-- @+<< Import needed modules >>
-- @+node:gcross.20101114125204.1260: ** << Import needed modules >>
import Control.Exception
import Control.Monad

import qualified Data.Sequence as Seq
import Data.Serialize

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Control.Monad.Trans.Visitor
import Control.Monad.Trans.Visitor.Path
-- @-<< Import needed modules >>

-- @+others
-- @-others

main = defaultMain
    -- @+<< Tests >>
    -- @+node:gcross.20101114125204.1267: ** << Tests >>
    -- @+others
    -- @+node:gcross.20110923164140.1187: *3* Control.Monad.Trans.Visitor
    [testGroup "Control.Monad.Trans.Visitor"
        -- @+others
        -- @+node:gcross.20110722110408.1173: *4* gatherVisitorResults
        [testGroup "gatherVisitorResults"
            -- @+others
            -- @+node:gcross.20110722110408.1177: *5* return
            [testCase "return" $ gatherVisitorResults (return ()) @?= [()]
            -- @+node:gcross.20110722110408.1174: *5* mzero
            ,testCase "mzero" $ gatherVisitorResults (mzero :: Visitor ()) @?= []
            -- @+node:gcross.20110722110408.1178: *5* mplus
            ,testCase "mplus" $ gatherVisitorResults (return 1 `mplus` return 2) @?= [1,2]
            -- @+node:gcross.20110722110408.1179: *5* cache
            ,testCase "cache" $ gatherVisitorResults (cache 42) @?= [42::Int]
            -- @-others
            ]
        -- @-others
        ]
    -- @+node:gcross.20110923164140.1188: *3* Control.Monad.Trans.Visitor.Path
    ,testGroup "Control.Monad.Trans.Visitor.Path"
        -- @+others
        -- @+node:gcross.20110923164140.1189: *4* walkVisitor
        [testGroup "walkVisitor"
            -- @+others
            -- @+node:gcross.20110923164140.1191: *5* null path
            [testCase "null path" $ (gatherVisitorResults . walkVisitor Seq.empty) (return 42) @?= [42]
            -- @+node:gcross.20110923164140.1200: *5* cache
            ,testCase "cache" $ do (gatherVisitorResults . walkVisitor (Seq.singleton (CacheStep (encode (42 :: Int))))) (cache (undefined :: Int)) @?= [42]
            -- @+node:gcross.20110923164140.1199: *5* choice
            ,testCase "choice" $ do
                (gatherVisitorResults . walkVisitor (Seq.singleton (ChoiceStep False))) (return 42 `mplus` undefined) @?= [42]
                (gatherVisitorResults . walkVisitor (Seq.singleton (ChoiceStep True))) (undefined `mplus` return 42) @?= [42]
            -- @+node:gcross.20110923164140.1192: *5* errors
            ,testGroup "errors"
                -- @+others
                -- @+node:gcross.20110923164140.1193: *6* DeadEnd (return)
                [testCase "DeadEnd (return)" $
                    try (
                        evaluate
                        .
                        gatherVisitorResults
                        $
                        walkVisitor (Seq.singleton (undefined :: VisitorStep)) (return (undefined :: Int))
                    ) >>= (@?= Left DeadEnd)
                -- @+node:gcross.20110923164140.1195: *6* DeadEnd (mzero)
                ,testCase "DeadEnd (mzero)" $
                    try (
                        evaluate
                        .
                        gatherVisitorResults
                        $
                        walkVisitor (Seq.singleton (undefined :: VisitorStep)) (mzero :: Visitor Int)
                    ) >>= (@?= Left DeadEnd)
                -- @+node:gcross.20110923164140.1196: *6* ChoiceStepAtCachePoint
                ,testCase "ChoiceStepAtCachePoint" $
                    try (
                        evaluate
                        .
                        gatherVisitorResults
                        $
                        walkVisitor (Seq.singleton (ChoiceStep undefined :: VisitorStep)) (cache undefined :: Visitor Int)
                    ) >>= (@?= Left ChoiceStepAtCachePoint)
                -- @+node:gcross.20110923164140.1198: *6* CacheStepAtChoicePoint
                ,testCase "CacheStepAtChoicePoint" $
                    try (
                        evaluate
                        .
                        gatherVisitorResults
                        $
                        walkVisitor (Seq.singleton (CacheStep undefined :: VisitorStep)) (undefined `mplus` undefined :: Visitor Int)
                    ) >>= (@?= Left CacheStepAtChoicePoint)
                -- @-others
                ]
            -- @-others
            ]
        -- @-others
        ]
    -- @-others
    -- @-<< Tests >>
    ]
-- @-leo
