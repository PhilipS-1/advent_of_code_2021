xMin = 253 
xMax = 280 
yMin = (-73)
yMax = (-46) 

main = do 
    print part1 
    print part2 

step :: ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int))
step  ((x, y), (xVel, yVel)) = ((x + xVel, y + yVel), (update xVel, yVel - 1)) where 
    update 0 = 0
    update xVel = if xVel > 0 then xVel -1 else xVel +1

trajectory :: (Int, Int) -> [((Int, Int), (Int, Int))]
trajectory (a, b) = takeWhile aboveTarget $ iterate step ((0,0), (a, b)) where 
    aboveTarget ((x,y), (xVel, yVel))
       | y < yMin  = False 
       | otherwise = True 

hitsTarget :: [((Int,Int), (Int,Int))] -> Bool 
hitsTarget xs = any inTargetArea xs where 
    inTargetArea ((x,y), (xVel, yVel))
        | x <= xMax && x >= xMin && y <= yMax && y >= yMin = True 
        | otherwise = False 

trajectories = [t |  x <- [1..xMax], y <-[yMin .. negate yMin], let t = trajectory (x, y), hitsTarget t ]

velosThatHit = [(x,y) |  x <- [1..xMax], y <-[yMin .. negate yMin], hitsTarget $ trajectory (x,y) ]

part1 = maximum . map snd . map fst . concat $ trajectories
part2 = length velosThatHit 