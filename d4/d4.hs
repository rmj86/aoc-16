{------------------------{  Advent of Code  -  Day N  }------------------------}
import Data.List (groupBy, sortBy, isInfixOf)
import Data.Ord (comparing)

data Room = Room {name :: String, sectorid :: Int, checksum :: String}
          deriving (Show)

{---------------------------------{  Part 1  }---------------------------------}

realRoom r = csum == checksum r
    where 
    csum = map fst . take 5 
            . sortBy (comparing ((0-) . snd))
            . sortBy (comparing fst)
            $ [(c, count c (name r)) | c <- ['a'..'z']]

count c str = length . filter (==c) $ str

part1 inp = sum . map sectorid . filter realRoom $ inp

{---------------------------------{  Part 2  }---------------------------------}

rotChar :: Int -> Char -> Char
rotChar _ '-' = '-'  -- preserve dashes in name
rotChar n c = toEnum . (+a) . (`mod` 26) . (+n) . (+(-a)) . fromEnum $ c
    where a = fromEnum 'a'

rotString n str = map (rotChar n) str

decode r = Room (rotString (sectorid r) (name r)) (sectorid r) (checksum r)

part2 inp = filter ((isInfixOf "pole").name) . map decode . filter realRoom $ inp

{-----------------------------------{  IO  }-----------------------------------}

parseRoom str = Room (init name) num ch
    where (name, numch) = break (`elem` ['0'..'9']) str
          [(num, ch')] = reads numch
          ch = init . tail $ ch'

splitOn y xs = groupBy (\a b-> if b==y then False else True ) xs

getData :: IO [Room]
getData = return . map parseRoom . lines =<< readFile "input"

main = do
    input <- getData
    putStrLn "Advent of Code - day N"
    putStr "  Part 1:\n    "
    print . part1 $ input
    putStr "  Part 2:\n    "
    traverse print . part2 $ input
