import           Control.Monad
import           Control.Monad.Loops
import qualified Data.Char           as C
import qualified Data.List           as L
import qualified Data.Set            as S

type CuMap = (S.Set (Int,Int), S.Set(Int,Int))

main = do
    (cumap, maxX, maxY) <- getInput "input.txt"
    print $ stepAll cumap (maxX,maxY) 1

stepAll :: CuMap -> (Int,Int) -> Int -> Int
stepAll (r,d) (mx,my) n  
    | S.size (S.intersection r nr) == rSize && S.size (S.intersection d nd) == dSize = n
    | otherwise = stepAll (nr,nd) (mx,my) (n+1)
    where
        nr = S.fromList [if stuck then (x,y) else (nextX,y) | 
            (x,y) <- S.toList r,
            let nextX = if x == mx then 0 else x+1,
            let stuck = (nextX,y) `S.member` d || (nextX,y) `S.member` r]
        nd = S.fromList [if stuck then (x,y) else (x,nextY) | 
            (x,y) <- S.toList d,
            let nextY = if y == my then 0 else y+1,
            let stuck = (x,nextY) `S.member` d || (x,nextY) `S.member` nr]

        rSize = S.size r
        dSize = S.size d

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
