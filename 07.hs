import Data.List.Split 

main = do 
    inputs <- map (read::String -> Int) . (splitOn ",") <$> readFile "inputs/07.txt"
    print $ minFuel inputs 
    print $ minFuel' inputs 

--part 1
minFuel :: [Int] -> Int 
minFuel xs = minimum $ map (distanceSum xs) [0.. maximum xs] where
    distanceSum xs a = foldr (\x acc-> abs (x-a) + acc) 0 xs 

--part 2 
gauß :: Int -> Int -> Int
gauß a b = abs (n *( n+1)) `div` 2 where 
        n = abs (a - b)

minFuel':: [Int] -> Int
minFuel' xs = minimum $ map (distanceSum xs) [0.. maximum xs] where
    distanceSum xs a = foldr (\x acc-> (gauß x a) + acc) 0 xs 

