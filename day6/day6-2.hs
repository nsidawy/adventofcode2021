import Control.Monad
import Text.Printf
import Data.List.Split

main = do  
    inputTimers <- getData "input.txt"
    let days = 128 
    --Run calculation for a single zero fish.
    let dailyTimers = run days [[0]]
    let dailyCounts = [length x | x <- dailyTimers]
    let countByAge = [sum $ [dailyCounts !! (days - t2) | t2 <- dailyTimers !! (days - t)] | t <- [0..8]] 
    let result = sum [countByAge !! i | i <- inputTimers]
    print result

--Run steps keeping track of fish at each day
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
