import System.IO  
import Control.Monad
import Data.Tuple.Select

data Movement = Up | Forward | Down

main = do  
    lines <- getLines "input.txt"
    let directions = map processLine lines
    let (p, d) = navigate directions (0, 0)
    let (p2, d2, _) = navigate2 directions (0, 0, 0)
    print $ p * d
    print $ p2 * d2

navigate :: [(Movement, Int)] -> (Int, Int) -> (Int, Int)
navigate [] (p, d) = (p, d)
navigate ((Forward, n) : ds) (p, d) = navigate ds (p + n, d)
navigate ((Down, n) : ds) (p, d) = navigate ds (p, d + n)
navigate ((Up, n) : ds) (p, d) = navigate ds (p, d - n)

navigate2 :: [(Movement, Int)] -> (Int, Int, Int) -> (Int, Int, Int)
navigate2 [] (p, d, a) = (p, d, a)
navigate2 ((Forward, n) : ds) (p, d, a) = navigate2 ds (p + n, d + a * n, a)
navigate2 ((Down, n) : ds) (p, d, a) = navigate2 ds (p, d, a + n)
navigate2 ((Up, n) : ds) (p, d, a) = navigate2 ds (p, d, a - n)

getLines :: String -> IO [String]
getLines path = lines <$> readFile path

processLine :: String -> (Movement, Int)
processLine line = (parseMovement $ head values, read $ last values)
    where
        values = words line

parseMovement :: String -> Movement
parseMovement s = case s of 
    "forward" -> Forward
    "up" -> Up
    "down" -> Down
    _ -> error "invalid movement"
