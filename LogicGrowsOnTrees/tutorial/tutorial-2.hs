import qualified Data.Sequence as Seq
import LogicGrowsOnTrees (exploreTree)
import LogicGrowsOnTrees.Examples.Queens (nqueensUsingBitsSolutions)

main = print . exploreTree . fmap Seq.singleton . nqueensUsingBitsSolutions $ 5