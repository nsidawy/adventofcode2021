import Control.Monad
import Text.Printf
import Data.List.Split
import qualified Data.Map as M
import Data.Time.Clock

type Point = (Int,Int)
type Coord = (Point,Point)

main = do  
    inputTimers <- getData "input.txt"
    let days = 128 
    --Run calculation for a single zero fish.
    let dailyTimers = run days [[0]]
    let dailyCounts = [length x | x <- dailyTimers]
    let result = sum $ concat [[dailyCounts !! (days - t2) | t2 <- dailyTimers !! (days - t)] | t <- inputTimers] 
    print result

--Run steps keeping track of fish and 
run :: Int -> [[Int]] -> [[Int]]
run 0 timers = timers
run i timers = run (i-1) (timers ++ [next])
    where 
        next = concatMap step (last timers)

step :: Int -> [Int]
step 0 = [6, 8]
step i = [i-1]

getData :: String -> IO [Int]
getData path = do
    lines <- lines <$> readFile path
    let timers = map read $ splitOn "," $ head lines
    return timers

parseLine :: String -> Coord
parseLine s = ((x1,y1),(x2,y2))
    where 
        [one, two] = splitOn " -> " s
        [x1, y1] = map read $ splitOn "," one
        [x2, y2] = map read $ splitOn "," two
