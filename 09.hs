import Data.Char (digitToInt)
import Data.Matrix 

main = do 
    inputs <-  map  words . lines <$> readFile "inputs/09.txt"
    let inputMatrix = fromLists (map (map digitToInt) $ concat inputs)
    print $ part1 inputMatrix

--part1
part1 ::  Matrix Int -> Int 
part1 b = foldr (\(a, b) acc -> if b then 1 + a + acc else 0 + acc) 0 (mapPos (\(r,c)a -> (a, lowerThanNeighbors r c a b)) b)

lowerThanNeighbors :: Int -> Int -> Int -> Matrix Int -> Bool
lowerThanNeighbors r c a b  = left && right && up && down  where 
    left = if c <= 1 then True else a < (getElem r (c-1) b)
    right = if c >= ncols b then True else a < (getElem r (c+1) b)
    up = if r <= 1 then True else a < (getElem (r-1) c b)
    down = if r >= nrows b then True else  a < (getElem (r+1) c b)