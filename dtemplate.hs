{------------------------{  Advent of Code  -  Day N  }------------------------}


{---------------------------------{  Part 1  }---------------------------------}

part1 inp = 0

{---------------------------------{  Part 2  }---------------------------------}

part2 inp = 0

{-----------------------------------{  IO  }-----------------------------------}

-- getData :: IO --
getData = return =<< readFile "input"

main = do
    input <- getData
    putStrLn "Advent of Code - day N"
    putStr "  Part 1:\n    "
    print . part1 $ input
    putStr "  Part 2:\n    "
    print . part2 $ input
