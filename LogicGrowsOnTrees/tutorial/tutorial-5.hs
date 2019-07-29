import LogicGrowsOnTrees (exploreTreeUntilFound)
import LogicGrowsOnTrees.Examples.Queens (nqueensUsingBitsSolutions)

main =
    print
    .
    exploreTreeUntilFound ((>= 3) . length)
    .
    fmap (:[])
    .
    nqueensUsingBitsSolutions
    $
    10
