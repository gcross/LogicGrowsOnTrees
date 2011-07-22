-- @+leo-ver=5-thin
-- @+node:gcross.20101114125204.1259: * @file tests.hs
-- @@language Haskell

-- @+<< Language extensions >>
-- @+node:gcross.20101114125204.1281: ** << Language extensions >>
{-# LANGUAGE UnicodeSyntax #-}
-- @-<< Language extensions >>

-- @+<< Import needed modules >>
-- @+node:gcross.20101114125204.1260: ** << Import needed modules >>
import Control.Arrow
import Control.Monad

import qualified Data.Sequence as Seq

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
    -- @+node:gcross.20101114125204.1275: *3* label
    [testGroup "label"
        -- @+others
        -- @+node:gcross.20101114125204.1277: *4* Maybe
        [testGroup "Maybe"
            -- @+others
            -- @+node:gcross.20101114125204.1278: *5* Nothing
            [testCase "Nothing" $ Nothing @=? label (visit (Nothing :: Maybe Int))
            -- @+node:gcross.20101114125204.1280: *5* Single choice
            ,testGroup "Single choice"
                [testCase (show a ++ " `mplus` " ++ show b) $
                    c @=? label (visit a `mplus` visit b)
                | (a,b,c) ←
                    [(Just 1,Just 2,Just (Seq.singleton L,1))
                    ,(Just 1,Nothing,Just (Seq.singleton L,1))
                    ,(Nothing,Just 2,Just (Seq.singleton R,2))
                    ]
                ]
            -- @-others
            ]
        -- @+node:gcross.20101114125204.1285: *4* List
        ,testGroup "List"
            -- @+others
            -- @+node:gcross.20101114125204.1286: *5* Nothing
            [testCase "[]" $ [] @=? label (visit ([] :: [Int]))
            -- @+node:gcross.20101114125204.1287: *5* Single choice
            ,testGroup "Single choice"
                [testCase (show a ++ " `mplus` " ++ show b) $
                    c @=? label (visit a `mplus` visit b)
                | (a,b,c) ←
                    [([1],[2],[(Seq.singleton L,1),(Seq.singleton R,2)])
                    ,([1,2],[3,4],[(Seq.singleton L,1),(Seq.singleton L,2),(Seq.singleton R,3),(Seq.singleton R,4)])
                    ]
                ]
            -- @+node:gcross.20101114125204.1288: *5* Two choices
            ,testGroup "Two choices"
                -- @+others
                -- @+node:gcross.20101114125204.1289: *6* #1
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
                -- @-others
                ]
            -- @-others
            ]
        -- @-others
        ]
    -- @+node:gcross.20101114125204.3782: *3* walkDownBranch
    ,testGroup "walkDownBranch"
        -- @+others
        -- @+node:gcross.20101114125204.3783: *4* Maybe
        [testGroup "Maybe"
            -- @+others
            -- @+node:gcross.20101114125204.3784: *5* Nothing
            [testCase "Nothing" $ Nothing @=? walkDownBranch Seq.empty (visit (Nothing :: Maybe Int))
            -- @+node:gcross.20101114125204.3785: *5* Single choice
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
            -- @-others
            ]
        -- @+node:gcross.20101114125204.3786: *4* List
        ,testGroup "List"
            -- @+others
            -- @+node:gcross.20101114125204.3787: *5* Nothing
            [testCase "[]" $ [] @=? walkDownBranch Seq.empty (visit ([] :: [Int]))
            -- @+node:gcross.20101114125204.3788: *5* Single choice
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
            -- @+node:gcross.20101114125204.3789: *5* Two choices
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
            -- @-others
            ]
        -- @-others
        ]
    -- @-others
    -- @-<< Tests >>
    ]
-- @-leo
