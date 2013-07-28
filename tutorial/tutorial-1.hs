import LogicGrowsOnTrees (exploreTree)
import LogicGrowsOnTrees.Examples.Queens (nqueensUsingBitsSolutions)

main = print . exploreTree . fmap (:[]) . nqueensUsingBitsSolutions $ 5
