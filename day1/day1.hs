import System.IO  
import Control.Monad

main = do  
    list <- getNumbers
    let answer1 = getIncreases list
    print answer1

getNumbers :: IO [Int]
getNumbers = do  
        let list = []
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        let singlewords = words contents
            list = toInt singlewords
        return list

getIncreases :: [Int] -> Int
getIncreases (x:y:xs) = (if x < y then 1 else 0) + getIncreases (y : xs) 
getIncreases _ = 0

toInt :: [String] -> [Int]
toInt = map read
