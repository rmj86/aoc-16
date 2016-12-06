{------------------------{  Advent of Code  -  Day 6  }------------------------}
import Data.List (transpose, sort, group, maximumBy, minimumBy)
import Data.Ord (comparing)
{---------------------------------{  Part 1  }---------------------------------}

mostCommon str = head . maximumBy (comparing length) . group . sort $ str

part1 inp = map mostCommon . transpose $ inp

{---------------------------------{  Part 2  }---------------------------------}

leastCommon str = head . minimumBy (comparing length) . group . sort $ str

part2 inp = map leastCommon . transpose $ inp

{-----------------------------------{  IO  }-----------------------------------}

-- getData :: IO --
getData = return . lines =<< readFile "input"

main = do
    input <- getData
    putStrLn "Advent of Code - day 6"
    putStr "  Part 1:\n    "
    print . part1 $ input
    putStr "  Part 2:\n    "
    print . part2 $ input
