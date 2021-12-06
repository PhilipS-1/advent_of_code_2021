import Data.List.Split
import Data.List

main = do 
    input <- splitOn "," <$> readFile "inputs/06.txt"
    let fish = map (read::String -> Int) input 
    let fishVector = startVector fish 
    print $ sum (spawn' fishVector 80)
    print $ sum (spawn' fishVector 256)
    
startVector :: [Int] -> [Int]
startVector l = [0] ++ (map length . group $ sort l) ++ [0,0,0] 

spawn' :: [Int] -> Int -> [Int]
spawn' (x:xs) n 
    | n == 0 = (x:xs)
    | otherwise = spawn' (zipWith (+) (xs ++ [x]) [0,0,0,0,0,0,x,0,0]) (n-1)