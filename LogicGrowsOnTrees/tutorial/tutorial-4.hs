import LogicGrowsOnTrees (exploreTreeUntilFirst)
import LogicGrowsOnTrees.Examples.Queens (nqueensUsingBitsSolutions)

main = print . exploreTreeUntilFirst . nqueensUsingBitsSolutions $ 10
