import Data.List.Split
import Data.List

main = do 
    input <- splitOn "," <$> readFile "inputs/06.txt"
    let fish = map (read::String -> Int) input 
    let fishVector = startVector fish 
    print $ sum . (!! 80) $ iterate spawn fishVector
    print $ sum . (!! 256) $ iterate spawn fishVector
    
startVector :: [Int] -> [Int]
startVector l = [0] ++ (map length . group $ sort l) ++ [0,0,0] 

spawn :: [Int] -> [Int]
spawn (x:xs)  = (zipWith (+) (xs ++ [x]) [0,0,0,0,0,0,x,0,0]) 