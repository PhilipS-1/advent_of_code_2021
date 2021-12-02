mapRead :: [String] -> [Int]
mapRead = map read 

part1 = do
    file <- readFile "inputs/01.txt"
    let input = mapRead $ words file
    print $ length $ filter (\(x,y) -> x < y ) (zip input (tail input))

part2 = do
    file <- readFile "inputs/01.txt"
    let input = mapRead $ words file
    print $ length $ filter (\(x,y) -> x < y) (zip input (drop 3 input))