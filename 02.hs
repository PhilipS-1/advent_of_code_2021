--part1
f :: String -> (String, Int)
f xs = g (words xs) 
    where g xs =  (head xs, read $ last xs)

part1 = do 
    file <- readFile "inputs/02.txt"
    let input =  map f . lines $ file 
    let result = foldr (\a b -> ((fst a ) + (fst b), (snd a) + (snd b)) ) (0,0) $ map dive $ input
    print $ (fst result) * (snd result)
    

dive :: (String, Int ) -> (Int, Int)
dive ("forward", x) = (x, 0)
dive ("up", y) = (0, -y)
dive ("down", y) = (0, y)



--part 2
get1 (a,_,_) = a
get2 (_,b,_) = b
get3 (_,_,c) = c

part2 = do 
    file <- readFile "inputs/02.txt"
    let input =  map f . lines $ file 
    let result = foldl (\a b -> addTriple a b) (0,0,0) $ map dive2 $ input
    --print $ (get1 result) * (get2 result)
    print $ (get1 result) * (get2 result)

--dive2 :: (String, Int) -> (x, y, aim)
dive2 :: (String, Int ) -> (Int, Int, Int)
dive2 ("forward", x) = (x, 1, 0)
dive2 ("up", y) = (0, 0, -y)
dive2 ("down", y) = (0, 0, y)

addTriple (x,y,aim) (x', y', aim') = (x + x', y + y' * (aim * x'), aim + aim')