import           Control.Monad
import           Control.Monad.Loops
import qualified Data.Char           as C
import qualified Data.List           as L
import qualified Data.Set            as S

type CuMap = (S.Set (Int,Int), S.Set(Int,Int))

main = do
    (cumap, maxX, maxY) <- getInput "input.txt"
    print (cumap,maxX,maxY)
    print $ stepAll cumap (maxX,maxY) 1

stepAll :: CuMap -> (Int,Int) -> Int -> Int
stepAll (r,d) m n  
    | S.size (S.intersection r nr) == rSize && S.size (S.intersection d nd) == dSize = n
    | otherwise = stepAll (nr,nd) m (n+1)
    where
        (nr,nd) = step (r,d) m
        rSize = S.size r
        dSize = S.size d

step :: CuMap -> (Int,Int) -> CuMap
step (r,d) (mx,my) = (nRight,nDown)
    where 
        nRight = stepRight (r,d) mx
        nDown = stepDown (nRight,d) my

stepRight :: CuMap -> Int -> S.Set (Int,Int)
stepRight (right,down) mx = nRight
    where
        nRight = S.fromList [if stuck then (x,y) else (nextX,y) | 
            (x,y) <- S.toList right,
            let nextX = if x == mx then 0 else x+1,
            let stuck = (nextX,y) `S.member` down || (nextX,y) `S.member` right]

stepDown :: CuMap -> Int -> S.Set (Int,Int)
stepDown (right,down) my = nDown
    where
        nDown = S.fromList [if stuck then (x,y) else (x,nextY) | 
            (x,y) <- S.toList down,
            let nextY = if y == my then 0 else y+1,
            let stuck = (x,nextY) `S.member` down || (x,nextY) `S.member` right]

getInput :: String -> IO (CuMap, Int, Int)
getInput path = do
    lines <- lines <$> readFile path
    let maxX = length (head lines) - 1
    let maxY = length lines - 1
    return (parseCuMap lines, maxX, maxY)

parseCuMap :: [String] -> CuMap
parseCuMap lines = (right, down)
    where 
        right = S.fromList $ concat [[(x,y) | (c,x) <- zip l [0..], c == '>'] | (l,y) <- zip lines [0..]]
        down = S.fromList $ concat [[(x,y) | (c,x) <- zip l [0..], c == 'v'] | (l,y) <- zip lines [0..]]
