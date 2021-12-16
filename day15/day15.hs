import           Control.Monad
import qualified Data.List       as L
import           Data.List.Split
import qualified Data.Vector        as V
import qualified Data.Map        as M
import qualified Data.Set        as S
import qualified Data.Ord        as O

type Grid = V.Vector (V.Vector Int)
type Memo = M.Map (Int, Int) Int
type Entered = S.Set (Int, Int)

main = do
    grid <- getInput "ex.txt"
    let (result, m) = findPath grid (0,0) M.empty
    print (result - (grid V.! 0 V.! 0)) 
    let (result, m) = findPath' grid (0,0) M.empty S.empty 0
    print (result - (grid V.! 0 V.! 0)) 

findPath :: Grid -> (Int, Int) -> Memo -> (Int, Memo)
findPath g (x,y) m
    -- reached end
    | x == V.length (g V.! 0) - 1 
        && y == V.length g - 1 = (current, m) 
    -- out of bounds
    | x >= V.length (g V.! 0)
        || y >= V.length g = (100000000, m)
    | (x,y) `M.member` m = (m M.! (x,y), m)
    | otherwise = (v', m')
    where
        current = (g V.! y) V.! x
        (v1, m1) = findPath g (x+1, y) m
        (v2, m2) = findPath g (x, y+1) m1
        v' = minimum [v1,v2] + current
        m' = M.insert (x,y) v' m2

findPath' :: Grid -> (Int, Int) -> Memo -> Entered -> Int -> (Int, Memo)
findPath' g (x,y) m s t
    -- reached end
    | x == V.length (g V.! 0) - 1
        && y == V.length g - 1 = (running, m)
    -- out of bounds
    | x >= V.length (g V.! 0)
        || y >= V.length g = (1000000000000, m)
    | (x,y) `S.member` s = (1000000000000, m)
    | (x,y) `M.member` m = if running > m M.! (x,y) then (1000000000000, m) else (m M.! (x,y), m)
    | otherwise = (v', m1')
    where
        current = (g V.! y) V.! x
        running = current + t
        m' = M.insert (x,y) (current + t) m
        s' = S.insert (x,y) s
        r1 = findPath' g (x+1, y) m s' running
        r2 = findPath' g (x, y+1) m s' running 
        r3 = findPath' g (x, y-1) m s' running 
        r4 = findPath' g (x-1, y) m s' running 
        ord (v1,m1) (v2,m2) = if v1 < v2 then O.LT else O.GT
        (v1, m1) = L.minimumBy ord [r1,r2,r3,r4]
        m1' = M.insert (x,y) (current + t) m1
        v' = v1 + current

getInput :: String -> IO Grid
getInput path = do
    lines <- lines <$> readFile path
    return $ V.fromList $ map (V.fromList . parseLine) lines 

parseLine :: String -> [Int]
parseLine = map (\c -> read [c])
