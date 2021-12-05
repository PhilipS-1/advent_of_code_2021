{-# LANGUAGE AllowAmbiguousTypes, TypeApplications #-} 

import Data.List.Split
import Data.List
import Data.Bool 

read' = (read @Int) 

main = do
    inputs <- map words <$> lines <$> readFile "inputs/05.txt"
    let lines = parseLines inputs 
    let hLines = onlyHorizontal lines 
    print $ solution $ map linePoints hLines
    print $ solution $ map linePoints lines 

solution :: [[(Int, Int)]] -> Int 
solution l =  length $ filter (>= 2) . (map length) . group $ sort $ concat l

parseLines :: [[String]] -> [[(Int, Int )]]
parseLines l =  map (map parsePoint) $ map (\s -> (head s) : [(last s)]) l

parsePoint :: String -> (Int, Int)
parsePoint s = ( read' $ head $ splitOn "," s, read' $ last $ splitOn "," s)

onlyHorizontal :: [[(Int, Int)]] -> [[(Int, Int)]]
onlyHorizontal = filter (\l -> sameX (head l) (last l) || sameY (head l) (last l))  where 
    sameX a b = fst a == fst b 
    sameY a b = snd a == snd b 

linePoints :: [(Int, Int)] -> [(Int, Int)]
linePoints [(x1, y1),(x2, y2)] 
        | x1 == x2 = [(x1,y) | y <- [(min y1 y2) .. (max y1 y2)]]
        | y1 == y2 = [(x,y1) | x <- [(min x1 x2) .. (max x1 x2)]]
        | otherwise = zip (coords x1 x2) (coords y1 y2) where 
            coords a b = bool [a,a-1 .. b] [a..b]  (a<b)
        