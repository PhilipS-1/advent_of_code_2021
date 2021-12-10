import Data.Char (digitToInt)
import Data.Matrix 
import Data.Function(on)
import Data.List 

main = do 
    inputs <-  map  words . lines <$> readFile "inputs/09.txt"
    let inputMatrix = fromLists (map (map digitToInt) $ concat inputs)
    print $ part1 inputMatrix
    print $ part2 inputMatrix
    
localMinima :: Matrix Int -> [(Int, Int)]
localMinima b = foldr (\(a, b) acc -> if b then a : acc else acc) [] (mapPos (\(r,c) a -> ((r,c), lowerThanNeighbors r c a b)) b)

lowerThanNeighbors :: Int -> Int -> Int -> Matrix Int -> Bool
lowerThanNeighbors r c a b  = left && right && up && down  where 
    left = if c <= 1 then True else a < (getElem r (c-1) b)
    right = if c >= ncols b then True else a < (getElem r (c+1) b)
    up = if r <= 1 then True else a < (getElem (r-1) c b)
    down = if r >= nrows b then True else  a < (getElem (r+1) c b)

getBasin :: [(Int, Int)] -> Matrix Int -> [(Int, Int)]
getBasin [] _ = []
getBasin basin b
    | sort (basinIteration basin b) == sort basin = basin 
    | otherwise = getBasin (basinIteration basin b) b 

basinIteration basin b = nub . concat $ (map (\(r,c) -> addNeighbors r c basin b) basin) 

addNeighbors :: Int -> Int -> [(Int, Int)] ->  Matrix Int -> [(Int, Int)]
addNeighbors r c basins b =  basins ++ left ++ right ++ up ++ down where 
    left = if isEligible (r-1) c basins b then [(r-1,c)] else []
    right = if isEligible (r+1) c basins b then [(r+1,c)] else []
    up = if isEligible r (c-1) basins b then [(r,c-1)] else []
    down = if isEligible r (c+1) basins b then [(r, c+1)] else []

isEligible :: Int -> Int -> [(Int, Int)] -> Matrix Int -> Bool 
isEligible 0 _ _ _ = False 
isEligible _ 0 _ _ = False
isEligible r c basins b 
    | r > nrows b = False 
    | c > ncols b = False 
    | (r,c) `elem` basins = False
    | getElem r c b >= 9 = False 
    | otherwise = True 

part1 ::  Matrix Int -> Int 
part1 b = sum $  map (\(r,c) -> (getElem r c b) + 1) (localMinima b)

part2 :: Matrix Int -> Int
part2 b = foldr (*) 1 $ take 3 $ reverse . sort $ map length $ map (\(r,c) -> getBasin [(r,c)] b) (localMinima b)