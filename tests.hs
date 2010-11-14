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
        -- @+node:gcross.20101114125204.1285:List
        ,testGroup "List"
            -- @    @+others
            -- @+node:gcross.20101114125204.1286:Nothing
            [testCase "[]" $ [] @=? label (visit ([] :: [Int]))
            -- @nonl
            -- @-node:gcross.20101114125204.1286:Nothing
            -- @+node:gcross.20101114125204.1287:Single branch
            ,testGroup "Single branch"
                [testCase (show a ++ " `mplus` " ++ show b) $
                    c @=? label (visit a `mplus` visit b)
                | (a,b,c) ←
                    [([1],[2],[(Seq.singleton L,1),(Seq.singleton R,2)])
                    ,([1,2],[3,4],[(Seq.singleton L,1),(Seq.singleton L,2),(Seq.singleton R,3),(Seq.singleton R,4)])
                    ]
                ]
            -- @nonl
            -- @-node:gcross.20101114125204.1287:Single branch
            -- @+node:gcross.20101114125204.1288:Two branches
            ,testGroup "Two branches"
                -- @    @+others
                -- @+node:gcross.20101114125204.1289:#1
                [testCase "#1" $
                    [(Seq.fromList [L,L],(0,0))
                    ,(Seq.fromList [L,R],(0,1))
                    ,(Seq.fromList [R,L],(1,0))
                    ,(Seq.fromList [R,R],(1,1))
                    ]
                    @=?
                    label
                    (do x ← (return 0) `mplus` (return 1)
                        y ← (return 0) `mplus` (return 1)
                        return (x,y)
                    )
                -- @nonl
                -- @-node:gcross.20101114125204.1289:#1
                -- @-others
                ]
            -- @-node:gcross.20101114125204.1288:Two branches
            -- @-others
            ]
        -- @nonl
        -- @-node:gcross.20101114125204.1285:List
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
