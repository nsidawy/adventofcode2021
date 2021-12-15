import           Control.Monad
import qualified Data.List       as L
import           Data.List.Split
import qualified Data.Vector        as V
import qualified Data.Map        as M

type Grid = V.Vector (V.Vector Int)
type Memo = M.Map (Int, Int) Int

main = do
    grid <- getInput "ex.txt"
    let (result, m) = findPath grid (0,0) M.empty
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

getInput :: String -> IO Grid
getInput path = do
    lines <- lines <$> readFile path
    return $ V.fromList $ map (V.fromList . parseLine) lines 

parseLine :: String -> [Int]
parseLine = map (\c -> read [c])
