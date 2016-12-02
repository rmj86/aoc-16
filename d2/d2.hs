{------------------------{  Advent of Code  -  Day 2  }------------------------}
import Data.Map.Strict (Map, fromList, (!), member)

{---------------------------------{  Part 1  }---------------------------------}

keypad = fromList $ zip [(x,y) | y<-[1..3], x<-[1..3]] "123456789"

step kp (x,y) s = if destination `member` kp
               then destination
               else (x,y)
  where destination = case s of
            'U' -> (x,  y-1)
            'D' -> (x,  y+1)
            'L' -> (x-1,  y)
            'R' -> (x+1,  y)

walk kp start instrs = foldl (step kp) start instrs

passcode kp = map (kp !) . tail . scanl (walk kp) (2,2)

part1 = passcode keypad

{---------------------------------{  Part 2  }---------------------------------}

real_keypad = fromList $ zip [(x,y) | y<-[1..5]
                                    , let w = min (6-y) y - 1
                                    , x<-[3-w .. 3+w]]
                             "123456789ABCD"

part2 = passcode real_keypad

{-----------------------------------{  IO  }-----------------------------------}

getData :: IO [String]
getData = return . lines =<< readFile "input"

main = do
    ls <- getData
    putStr "Part 1:\n  "
    putStrLn . show . part1 $ ls
    putStr "Part 2:\n  "
    putStrLn . show . part2 $ ls
