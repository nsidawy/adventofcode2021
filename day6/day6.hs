import Control.Monad
import Text.Printf
import Data.List.Split
import qualified Data.Map as M

type Point = (Int,Int)
type Coord = (Point,Point)

main = do  
    timers <- getData "input.txt"
    let days = 256 
    end <- run days timers
    print $ length end

run :: Int -> [Int] -> IO [Int]
run 0 timers = do return timers
run i timers = do
    print i
    run (i-1) (concatMap step timers)

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
