-- @+leo-ver=5-thin
-- @+node:gcross.20101114125204.1259: * @file tests.hs
-- @@language haskell

-- @+<< Language extensions >>
-- @+node:gcross.20101114125204.1281: ** << Language extensions >>
{-# LANGUAGE UnicodeSyntax #-}
-- @-<< Language extensions >>

-- @+<< Import needed modules >>
-- @+node:gcross.20101114125204.1260: ** << Import needed modules >>
import Control.Monad

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Control.Monad.Trans.Visitor
-- @-<< Import needed modules >>

-- @+others
-- @-others

main = defaultMain
    -- @+<< Tests >>
    -- @+node:gcross.20101114125204.1267: ** << Tests >>
    -- @+others
    -- @+node:gcross.20110722110408.1173: *3* gatherVisitorResults
    [testGroup "gatherVisitorResults"
        -- @+others
        -- @+node:gcross.20110722110408.1177: *4* return
        [testCase "return" $ gatherVisitorResults (return ()) @?= [()]
        -- @+node:gcross.20110722110408.1174: *4* mzero
        ,testCase "mzero" $ gatherVisitorResults (mzero :: Visitor ()) @?= []
        -- @+node:gcross.20110722110408.1178: *4* mplus
        ,testCase "mplus" $ gatherVisitorResults (return 1 `mplus` return 2) @?= [1,2]
        -- @+node:gcross.20110722110408.1179: *4* cache
        ,testCase "cache" $ gatherVisitorResults (cache 42) @?= [42::Int]
        -- @-others
        ]
    -- @-others
    -- @-<< Tests >>
    ]
-- @-leo
