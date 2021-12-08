import Data.List.Split
import Data.List 
import qualified Data.Map.Strict as M 

main = do 
    inputs <-  (map (splitOn "|") . lines) <$> readFile "inputs/08.txt"
    let signals = map parseSignals inputs 
        digits =  map parseDigits inputs 
        part2Input = zip signals digits 
    print $ part1 digits
    print $ sum $ map (digitsToInt . solvePart2) part2Input


parseSignals :: [String] -> [String]
parseSignals [signals, digits] = words signals  

parseDigits :: [String] ->  [String] 
parseDigits [signals, digits] = (take 4 . words) digits 

part1 :: [[String]] -> Int 
part1 digits =  length $ filter (\x -> (x == 2) ||( x ==3) ||( x ==4) || (x == 7)) $  map length ( concat digits)

--part2
solvePart2 :: ([String],[String]) -> [Int] 
solvePart2 (signals, digits) = map (\s -> decoder M.! (sort s)) digits where 
    decoder = decodeSignals signals  

decodeSignals :: [String] -> M.Map String Int
decodeSignals signals = M.fromList [(zero, 0), (one, 1), (two, 2), (three, 3), (four, 4), (five, 5), (six, 6), (seven, 7), (eight, 8), (nine, 9)] where 
    one = sort . get $ filter ((==2) . length) signals
    four = sort . get $ filter ((== 4) . length) signals
    seven = sort . get $ filter ((== 3) . length) signals 
    eight = sort . get $ filter ((== 7) . length) signals 
    length5 = map sort $ filter ((== 5) . length) signals 
    length6 = map sort $ filter ((== 6) . length) signals
    three = sort . get $ filter (allElem one . sort) length5
    nine = sort . get $ filter (allElem three . sort) length6
    six = sort . get $ filter (not . allElem one . sort) length6
    zero = sort . get $ (length6 \\ [nine, six ])
    five = sort . get $ filter (\s -> allElem (sort s) six) length5
    two = sort . get $ (length5 \\ [three, five ])
    
get :: [String] -> String 
get [x] = x 
get (x:xs) =  "error"
get [] = "empty list"

allElem :: String -> String -> Bool 
allElem [] s = True 
allElem (x:xs) s = elem x s && allElem xs s  

digitsToInt :: [Int] -> Int 
digitsToInt digits = sum $ zipWith toDecimal (reverse digits) [0..3] where
    toDecimal a b = a * (10 ^ b) 

    


