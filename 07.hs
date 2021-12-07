import Data.List.Split 

main = do 
    inputs <- map (read::String -> Int) . (splitOn ",") <$> readFile "inputs/07.txt"
    print $ calcFuelWith (-) inputs 
    print $ calcFuelWith gauß inputs 

calcFuelWith :: (Int -> Int -> Int) -> [Int] -> Int 
calcFuelWith f xs = minimum $ map (distanceSum xs) [0.. maximum xs] where
    distanceSum xs a = sum $ map (abs . f a) xs 

gauß :: Int -> Int -> Int
gauß a b = abs (n *( n+1)) `div` 2 where 
        n = abs (a - b)


