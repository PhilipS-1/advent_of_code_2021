import Data.List.Split
import Data.List 

main = do 
    inputs <-  (map (splitOn "|") . lines) <$> readFile "inputs/08.txt"
    let signals = map parseSignals inputs 
        digits =  map parseDigits inputs 
    --print signals
    print $ part1 digits

parseSignals :: [String] -> [String]
parseSignals [signals, digits] = words signals  

parseDigits :: [String] ->  [String] 
parseDigits [signals, digits] = (take 4 . words) digits 

part1 :: [[String]] -> Int 
part1 digits =  length $ filter (\x -> (x == 2) ||( x ==3) ||( x ==4) || (x == 7)) $  map length ( concat digits)