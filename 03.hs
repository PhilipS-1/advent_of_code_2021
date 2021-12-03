import Data.List 
import Data.Char(digitToInt)

--part 1
part1 = do
    file <- readFile "inputs/03.txt"
    let input = transpose $ words file 
    let bits = map averageBit input 
    let gamma = binaryToInt bits 
    let epsilon = binaryToInt $ getEpsilon bits 
    print gamma 
    print epsilon 
    print $ gamma * epsilon

averageBit :: [Char] -> Char 
averageBit s 
    | (length $ filter (=='1') s) >= (length $ filter (=='0') s) = '1' 
    | otherwise = '0' 

binaryToInt :: String -> Int 
binaryToInt s = sum $ zipWith toBin (reverse s) [0.. length s]
  where toBin a b =  (digitToInt a) * (2 ^ b )

getEpsilon :: String -> String 
getEpsilon (x:xs)
    | x == '1' = '0' : getEpsilon xs 
    | otherwise = '1' : getEpsilon xs 
getEpsilon [] = []

--part 2 
part2 = do 
    file <- readFile "inputs/03.txt"
    let input = words file
    --print input 
    let oxygenRating = head $ map binaryToInt $ oxygen 0 input 
    let co2Rating = head $ map binaryToInt $ co2 0 input 
    print $ oxygenRating * co2Rating

bitAt :: Int -> [[b]] -> [b]
bitAt x = map (!! x) 

oxygen :: Int -> [String] -> [String]
oxygen k l 
    | length l == 1 = l 
    | otherwise = oxygen (k+1) (filter (\s -> s!!k == (mostCommonBit k l)) l)
        where mostCommonBit n s = averageBit $ bitAt n s 

co2 :: Int -> [String] -> [String]
co2 k l 
    | length l == 1 = l 
    | otherwise = co2 (k+1) (filter (\s -> s!!k == (leastCommonBit k l)) l) where
         leastCommonBit n s 
            | (averageBit $ bitAt n s ) == '1' = '0'
            | otherwise = '1' 