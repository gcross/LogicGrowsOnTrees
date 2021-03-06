{-# LANGUAGE UnicodeSyntax #-}

import Criterion.Main

import LogicGrowsOnTrees
import LogicGrowsOnTrees.Checkpoint
import LogicGrowsOnTrees.Utils.WordSum
import LogicGrowsOnTrees.Parallel.Common.Worker (exploreTreeGeneric)
import LogicGrowsOnTrees.Parallel.ExplorationMode (ExplorationMode(AllMode))
import LogicGrowsOnTrees.Parallel.Purity (Purity(Pure))

unitBindIt :: Monad m ⇒ Int → m WordSum
unitBindIt 0 = return (WordSum 1)
unitBindIt depth = return () >> unitBindIt (depth-1)
{-# NOINLINE unitBindIt #-}

intBindIt :: Monad m ⇒ Int → m WordSum
intBindIt 0 = return (WordSum 1)
intBindIt depth = return (depth-1) >>= intBindIt
{-# NOINLINE intBindIt #-}

main :: IO ()
main = defaultMain
    [bgroup "list"
        [bench "unit" $ nf (getWordSum . mconcat . unitBindIt) depth
        ,bench "int" $ nf (getWordSum . mconcat . intBindIt) depth
        ]
    ,bgroup "tree"
        [bench "unit" $ nf (getWordSum . exploreTree . unitBindIt) depth
        ,bench "int" $ nf (getWordSum . exploreTree . intBindIt) depth
        ]
    ,bgroup "tree w/ checkpointing"
        [bench "unit" $ nf (getWordSum . exploreTreeStartingFromCheckpoint Unexplored . unitBindIt) depth
        ,bench "int" $ nf (getWordSum . exploreTreeStartingFromCheckpoint Unexplored . intBindIt) depth
        ]
    ,bgroup "tree using worker"
        [bench "unit" $ nfIO (doWorker unitBindIt depth)
        ,bench "int" $ nfIO (doWorker intBindIt depth)
        ]
    ]
  where
    depth = 16384

    doWorker lopsidedTree tree_depth =
        exploreTreeGeneric AllMode Pure (lopsidedTree tree_depth :: Tree WordSum)
    {-# NOINLINE doWorker #-}
