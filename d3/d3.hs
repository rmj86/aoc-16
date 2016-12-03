{------------------------{  Advent of Code  -  Day 3  }------------------------}
import Data.List (sort, unfoldr, transpose)

validTri [a,b,c] = x+y > z
    where
    [x,y,z] = sort [a,b,c]

{---------------------------------{  Part 1  }---------------------------------}

part1 inp = length . filter validTri $ inp

{---------------------------------{  Part 2  }---------------------------------}

transpose__ trips = concatMap transpose . groupsOf 3 $ trips
    where
    groupsOf n xs = unfoldr (\ts-> if null ts then Nothing else Just (splitAt n ts)) xs

part2 inp = length . filter validTri . transpose__ $ inp

{-----------------------------------{  IO  }-----------------------------------}

getData :: IO [[Int]]
getData = return . map (map read . words) . lines =<< readFile "input"

main = do
    input <- getData
    putStrLn "Advent of Code - day 3"
    putStr "  Part 1:\n    "
    print . part1 $ input
    putStr "  Part 2:\n    "
    print . part2 $ input
