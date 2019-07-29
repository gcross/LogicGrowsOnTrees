import LogicGrowsOnTrees (exploreTree)
import LogicGrowsOnTrees.Utils.WordSum (WordSum(..))
import LogicGrowsOnTrees.Examples.Queens (nqueensUsingBitsSolutions)

main = print . exploreTree . fmap (const $ WordSum 1) . nqueensUsingBitsSolutions $ 5
