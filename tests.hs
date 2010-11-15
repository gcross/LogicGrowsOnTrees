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
import Control.Arrow
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
            -- @+node:gcross.20101114125204.1280:Single choice
            ,testGroup "Single choice"
                [testCase (show a ++ " `mplus` " ++ show b) $
                    c @=? label (visit a `mplus` visit b)
                | (a,b,c) ←
                    [(Just 1,Just 2,Just (Seq.singleton L,1))
                    ,(Just 1,Nothing,Just (Seq.singleton L,1))
                    ,(Nothing,Just 2,Just (Seq.singleton R,2))
                    ]
                ]
            -- @nonl
            -- @-node:gcross.20101114125204.1280:Single choice
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
            -- @+node:gcross.20101114125204.1287:Single choice
            ,testGroup "Single choice"
                [testCase (show a ++ " `mplus` " ++ show b) $
                    c @=? label (visit a `mplus` visit b)
                | (a,b,c) ←
                    [([1],[2],[(Seq.singleton L,1),(Seq.singleton R,2)])
                    ,([1,2],[3,4],[(Seq.singleton L,1),(Seq.singleton L,2),(Seq.singleton R,3),(Seq.singleton R,4)])
                    ]
                ]
            -- @nonl
            -- @-node:gcross.20101114125204.1287:Single choice
            -- @+node:gcross.20101114125204.1288:Two choices
            ,testGroup "Two choices"
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
            -- @nonl
            -- @-node:gcross.20101114125204.1288:Two choices
            -- @-others
            ]
        -- @nonl
        -- @-node:gcross.20101114125204.1285:List
        -- @-others
        ]
    -- @nonl
    -- @-node:gcross.20101114125204.1275:label
    -- @+node:gcross.20101114125204.3782:walkDownBranch
    ,testGroup "walkDownBranch"
        -- @    @+others
        -- @+node:gcross.20101114125204.3783:Maybe
        [testGroup "Maybe"
            -- @    @+others
            -- @+node:gcross.20101114125204.3784:Nothing
            [testCase "Nothing" $ Nothing @=? walkDownBranch Seq.empty (visit (Nothing :: Maybe Int))
            -- @nonl
            -- @-node:gcross.20101114125204.3784:Nothing
            -- @+node:gcross.20101114125204.3785:Single choice
            ,testGroup "Single choice"
                [testCase (show a ++ " `mplus` " ++ show b) $
                    d @=? walkDownBranch c (visit a `mplus` visit b)
                | (a,b,c,d) ←
                    [(Just 1,Just 2,Seq.singleton L,Just 1)
                    ,(Just 1,Just 2,Seq.singleton R,Just 2)
                    ,(Just 1,Nothing,Seq.singleton L,Just 1)
                    ,(Just 1,Nothing,Seq.singleton R,Nothing)
                    ,(Nothing,Just 2,Seq.singleton L,Nothing)
                    ,(Nothing,Just 2,Seq.singleton R,Just 2)
                    ]
                ]
            -- @nonl
            -- @-node:gcross.20101114125204.3785:Single choice
            -- @-others
            ]
        -- @nonl
        -- @-node:gcross.20101114125204.3783:Maybe
        -- @+node:gcross.20101114125204.3786:List
        ,testGroup "List"
            -- @    @+others
            -- @+node:gcross.20101114125204.3787:Nothing
            [testCase "[]" $ [] @=? walkDownBranch Seq.empty (visit ([] :: [Int]))
            -- @nonl
            -- @-node:gcross.20101114125204.3787:Nothing
            -- @+node:gcross.20101114125204.3788:Single choice
            ,testGroup "Single choice"
                [testCase (show a ++ " `mplus` " ++ show b) $
                    d @=? walkDownBranch c (visit a `mplus` visit b)
                | (a,b,c,d) ←
                    [([1],[2],Seq.fromList [L,L],[1])
                    ,([1],[2],Seq.fromList [L,R],[1])
                    ,([1],[2],Seq.fromList [R,L],[2])
                    ,([1],[2],Seq.fromList [R,R],[2])
                    ,([1,2],[3,4],Seq.fromList [L,L,L],[1,2])
                    ,([1,2],[3,4],Seq.fromList [R,R,R],[3,4])
                    ]
                ]
            -- @nonl
            -- @-node:gcross.20101114125204.3788:Single choice
            -- @+node:gcross.20101114125204.3789:Two choices
            ,testGroup "Two choices" $
                let m = do  x ← (return 0) `mplus` (return 1)
                            y ← (return 0) `mplus` (return 1)
                            return (x,y)
                in
                [testCase (show branch) $ correct @=? walkDownBranch branch m
                | (branch,correct) ← map (first Seq.fromList)
                    [([L],[(0,0),(0,1)])
                    ,([R],[(1,0),(1,1)])
                    ,([L,L],[(0,0)])
                    ,([L,R],[(0,1)])
                    ,([R,L],[(1,0)])
                    ,([R,R],[(1,1)])
                    ]
                ]
            -- @nonl
            -- @-node:gcross.20101114125204.3789:Two choices
            -- @-others
            ]
        -- @nonl
        -- @-node:gcross.20101114125204.3786:List
        -- @-others
        ]
    -- @nonl
    -- @-node:gcross.20101114125204.3782:walkDownBranch
    -- @-others
    -- @nonl
    -- @-node:gcross.20101114125204.1267:<< Tests >>
    -- @nl
    ]
-- @-node:gcross.20101114125204.1259:@thin tests.hs
-- @-leo
