{-# LANGUAGE AllowAmbiguousTypes, TypeApplications #-} 

import Data.List.Split
import Data.List

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
linePoints l = getPoints x1 x2 y1 y2 where 
    x1 = fst $ head l 
    x2 = fst $ last l 
    y1 = snd $ head l 
    y2 = snd $ last l 
    getPoints x1 x2 y1 y2 
        | x1 == x2 = [(x1,y) | y <- [(min y1 y2) .. (max y1 y2)]]
        | y1 == y2 = [(x,y1) | x <- [(min x1 x2) .. (max x1 x2)]]
        | x1 < x2 && y1 < y2 = zip [x1..x2] [y1 .. y2]
        | x1 < x2 && y1 > y2 = zip [x1..x2] [y1, (y1-1) .. y2]
        | x1 > x2 && y1 < y2 = zip [x1, (x1-1)..x2] [y1 .. y2]
        | x1 > x2 && y1 > y2 = zip [x1, (x1-1) .. x2] [y1, (y1-1) .. y2]