import           Control.Monad
import           Data.List.Split
import qualified Data.Vector     as V
import           Text.Printf

main = do
    pos <- getData "input.txt"
    let min = minimum pos
    let max = maximum pos
    let result = minimum [run t pos | t <- [min..max]]
    let fuelCost = V.fromList $ getFuelCost max
    let result2 = minimum [run2 t pos fuelCost | t <- [min..max]]
    print result
    print result2

run :: Int -> [Int] -> Int
run target pos = sum [abs $ x - target | x <- pos]

run2 :: Int -> [Int] -> V.Vector Int -> Int
run2 target pos fuelCost = sum [fuelCost V.! abs (x - target) | x <- pos]

getFuelCost :: Int -> [Int]
getFuelCost 0 = [0]
getFuelCost x = fuelCost ++ [x + last fuelCost]
    where 
        fuelCost = getFuelCost $ x-1

getData :: String -> IO [Int]
getData path = do
    lines <- lines <$> readFile path
    let pos = map read $ splitOn "," $ head lines
    return pos
