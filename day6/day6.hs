import           Control.Monad
import           Data.List.Split
import qualified Data.Vector     as V
import           Text.Printf

main = do
    inputTimers <- getData "input.txt"
    let days = 256
    let daysHalf = days `div` 2
    --Run calculation for a single zero fish.
    let dailyTimers = run daysHalf [[0]]
    let dailyCounts = [length x | x <- dailyTimers]
    let countByAge = [sum $ [dailyCounts !! (daysHalf - t2) | t2 <- dailyTimers !! (daysHalf - t)] | t <- [0..8]]
    let result = sum [countByAge !! i | i <- inputTimers]
    print result

    let fishes = V.fromList [length $ filter (== i) inputTimers | i <- [0..8]]
    print $ V.sum $ runAgg days fishes

--Run steps keeping track of fish at each day
run :: Int -> [[Int]] -> [[Int]]
run 0 timers = timers
run i timers = run (i-1) (timers ++ [next])
    where
        next = concatMap step (last timers)

step :: Int -> [Int]
step 0 = [6, 8]
step i = [i-1]

runAgg :: Int -> V.Vector Int -> V.Vector Int
runAgg 0 timers = timers
runAgg i timers = runAgg (i-1) $ V.fromList [
        timers V.! 1
        , timers V.! 2
        , timers V.! 3
        , timers V.! 4
        , timers V.! 5
        , timers V.! 6
        , timers V.! 7 + timers V.! 0
        , timers V.! 8
        , timers V.! 0
    ]

getData :: String -> IO [Int]
getData path = do
    lines <- lines <$> readFile path
    let timers = map read $ splitOn "," $ head lines
    return timers
