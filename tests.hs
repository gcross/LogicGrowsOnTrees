-- @+leo-ver=4-thin
-- @+node:gcross.20101114125204.1259:@thin tests.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20101114125204.1281:<< Language extensions >>
{-# LANGUAGE UnicodeSyntax #-}
-- @nonl
-- @-node:gcross.20101114125204.1281:<< Language extensions >>
-- @nl

-- @<< Import needed modules >>
-- @+node:gcross.20101114125204.1260:<< Import needed modules >>
import Control.Monad

import qualified Data.Sequence as Seq

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Control.Monad.Trans.Visitor
-- @nonl
-- @-node:gcross.20101114125204.1260:<< Import needed modules >>
-- @nl

-- @+others
-- @-others

main = defaultMain
    -- @    << Tests >>
    -- @+node:gcross.20101114125204.1267:<< Tests >>
    -- @+others
    -- @+node:gcross.20101114125204.1275:label
    [testGroup "label"
        -- @    @+others
        -- @+node:gcross.20101114125204.1277:Maybe
        [testGroup "Maybe"
            -- @    @+others
            -- @+node:gcross.20101114125204.1278:Nothing
            [testCase "Nothing" $ Nothing @=? label (visit (Nothing :: Maybe Int))
            -- @nonl
            -- @-node:gcross.20101114125204.1278:Nothing
            -- @+node:gcross.20101114125204.1280:Single branch
            ,testGroup "Single branch"
                [testCase (show a ++ " `mplus` " ++ show b) $
                    c @=? label (visit a `mplus` visit b)
                | (a,b,c) ←
                    [(Just 1,Just 2,Just (Seq.singleton L,1))
                    ,(Just 1,Nothing,Just (Seq.singleton L,1))
                    ,(Nothing,Just 2,Just (Seq.singleton R,2))
                    ]
                ]
            -- @nonl
            -- @-node:gcross.20101114125204.1280:Single branch
            -- @-others
            ]
        -- @nonl
        -- @-node:gcross.20101114125204.1277:Maybe
        -- @-others
        ]
    -- @nonl
    -- @-node:gcross.20101114125204.1275:label
    -- @-others
    -- @nonl
    -- @-node:gcross.20101114125204.1267:<< Tests >>
    -- @nl
    ]
-- @-node:gcross.20101114125204.1259:@thin tests.hs
-- @-leo
