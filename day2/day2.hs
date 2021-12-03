import System.IO  
import Control.Monad

main = do  
    list <- getNumbers
    print $ getIncreases list
    print $ getIncreases2 list

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

getIncreases2 :: [Int] -> Int
getIncreases2 (v1:v2:v3:v4:xs) = (if v1 + v2 + v3 < v2 + v3 + v4 then 1 else 0) + getIncreases2 (v2 : v3 : v4 : xs) 
getIncreases2 _ = 0

toInt :: [String] -> [Int]
toInt = map read
