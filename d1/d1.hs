{---------------------{  Advent of Code 2016  -  Day 1  }----------------------}

data Turn = L | R  deriving (Show)
data Direction = N | E | S | W  deriving (Show, Enum, Eq)
type Instruction = (Turn, Int)

v L = -1
v R = 1

toDirs instrs = tail $ scanl next (N,0) instrs
    where
    next (d,_) (t,n) = (toEnum . (\i-> mod (i+v t) 4) . fromEnum $ d,  n)
    
dist (x,y) = abs(x) + abs(y)  -- manhattan distance

{---------------------------------{  Part 1  }---------------------------------}

totalDistance instrs = dist (e-w, n-s)
    where
    n = sum [n | (d,n)<-instrs, d==N]
    e = sum [n | (d,n)<-instrs, d==E]
    s = sum [n | (d,n)<-instrs, d==S]
    w = sum [n | (d,n)<-instrs, d==W]

{---------------------------------{  Part 2  }---------------------------------}

waypoints dirs = scanl step (0,0) dirs
    where
    step (x,y) (d,n) = case d of
        N  ->  (x,  y+n)
        S  ->  (x,  y-n)
        E  ->  (x+n,  y)
        W  ->  (x-n,  y)

path dirs = (0,0) : (concat $ zipWith travel (waypoints dirs) dirs)
    where
    travel (x,y) (d,n) = case d of
        N  ->  [(x,  y+i) | i<-[1..n]]
        S  ->  [(x,  y-i) | i<-[1..n]]
        E  ->  [(x+i,  y) | i<-[1..n]]
        W  ->  [(x-i,  y) | i<-[1..n]]

firstDuplicate xs = go [] xs
    where
    go visited (y:ys)
      | y `elem` visited = y
      | otherwise = go (y:visited) ys

{-----------------------------------{  IO  }-----------------------------------}

getData :: IO [Instruction]
getData = return . map parseInstr . words . filter (/=',') =<< readFile "input"
    where parseInstr s = case s of
            'L':n -> (L, (read n))
            'R':n -> (R, (read n))

main = do
    instrs <- getData
    let dirs = toDirs instrs
    print . totalDistance $ dirs
    print . dist . firstDuplicate . path $ dirs
