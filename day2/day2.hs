import System.IO  
import Control.Monad
import Data.List.Split
import Data.Tuple.Select

data Movement = Up | Forward | Down

main = do  
    lines <- getLines "input.txt"
    let directions = map processLine lines
    let result = navigate directions (0, 0)
    let result2 = navigate2 directions (0, 0, 0)
    print result
    print $ (fst result) * (snd result)
    print result2
    print $ (sel1 result2) * (sel2 result2)

navigate :: [(String, Int)] -> (Int, Int) -> (Int, Int)
navigate [] (p, d) = (p, d)
navigate (("forward", n) : ds) (p, d) = navigate ds (p + n, d)
navigate (("down", n) : ds) (p, d) = navigate ds (p, d + n)
navigate (("up", n) : ds) (p, d) = navigate ds (p, d - n)
navigate _ _ = (0,0)

navigate2 :: [(String, Int)] -> (Int, Int, Int) -> (Int, Int, Int)
navigate2 [] (p, d, a) = (p, d, a)
navigate2 (("forward", n) : ds) (p, d, a) = navigate2 ds (p + n, d + a * n, a)
navigate2 (("down", n) : ds) (p, d, a) = navigate2 ds (p, d, a + n)
navigate2 (("up", n) : ds) (p, d, a) = navigate2 ds (p, d, a - n)
navigate2 _ _ = (0,0,0)

getLines :: String -> IO [String]
getLines path = lines <$> readFile path

processLine :: String -> (String, Int)
processLine line = (head values, read $ last values)
    where
        values = splitOn " " line

parseMovement :: String -> Movement
parseMovement s = case s of 
    "forward" -> Forward
    "up" -> Up
    "down" -> Down
    _ -> error "invalid movement"
