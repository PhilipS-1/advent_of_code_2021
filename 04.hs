{-# LANGUAGE TypeApplications #-}

import Data.List.Split 
import Data.List

main = do 
    inputs <- lines <$> readFile ("inputs/04.txt")
    let drawOrder = map (read @Int) $ splitOn "," $ head inputs 
        boards = map Board $ map (map (map NotMarked)) $ parseBoards . chunksOf 6 $ tail inputs 
    print drawOrder 
    print boards
    print $ playBingo drawOrder boards (-1)
    print $ playBingo2 drawOrder boards (-1)

parseBoards = convertToInt . map (map words . drop 1) 
    where convertToInt = map( map ( map (read @Int)))

data BoardNumber = Marked Int | NotMarked Int deriving (Show, Eq)  

isMarked :: BoardNumber -> Bool 
isMarked (Marked _) = True
isMarked (NotMarked _) = False 

toInt :: BoardNumber -> Int 
toInt (Marked n) = n
toInt (NotMarked n) = n  

data Board = Board {rows :: [[BoardNumber]] } deriving Show 

getScore :: Board -> Int
getScore b = sum $ map toInt $ filter( not . isMarked ) $ concat $ rows b

drawAndMark :: Int -> Board -> Board  
drawAndMark n b = Board $ chunksOf 5 $ map (draw n) $ concat $ rows b where 
    draw n (NotMarked i) 
        | i == n = (Marked i)
        | otherwise = (NotMarked i)
    draw n (Marked i) = (Marked i)

bingoRow :: [BoardNumber] -> Bool 
bingoRow  b = 5 == (length $ filter isMarked b)

bingoBoard :: Board -> Bool 
bingoBoard b = (foldl (||) False (map bingoRow (rows b))) || (foldl (||) False (map bingoRow (transpose $ rows b)))

playBingo :: [Int] -> [Board] -> Int -> Int 
playBingo [] _ _ = -1
playBingo (x:xs) bs n 
    | 1 == (length $ filter bingoBoard bs) = n *( sum $ map getScore $ filter bingoBoard bs )
    | otherwise = playBingo xs (map (drawAndMark x) bs) x 

playBingo2 :: [Int] -> [Board] -> Int -> Int 
playBingo2 [] _ _ = -1 
playBingo2 (x:xs) bs n 
    | 1 == (length $ filter (not . bingoBoard) bs) = ( sum $ map (markRemaining (x:xs) n) $ filter (not . bingoBoard) bs)
    | otherwise = playBingo2 xs (map (drawAndMark x) bs) x 

markRemaining :: [Int] -> Int -> Board -> Int 
markRemaining [] n b = -1 
markRemaining (x:xs) n b     
    | bingoBoard b = n * getScore b
    | otherwise = markRemaining xs x (drawAndMark x b)