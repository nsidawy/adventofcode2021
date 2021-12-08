import           Control.Monad
import           Data.List.Split
import           Text.Printf
import qualified Data.List as L

main = do
    dat <- getOutput "input.txt"
    let outputs = concat [snd d | d <- dat]
    let smallCount = length $ filter (\n -> n == 2 || n == 3 || n == 4 || n == 7) $ map length outputs
    print $ sum $ map process dat

process :: ([String], [String]) -> Int
process (inputs, outputs) = sum $ zipWith (\d n -> n * getInt mapping d) sortedOutput [1000, 100, 10, 1] 
    where 
        letters = ['a', 'b', 'c', 'd', 'e', 'f', 'g']
        -- sort inputs and ouputs so we have one string per digit
        sortedInput = map L.sort inputs
        sortedOutput = map L.sort outputs
        distinct = L.nub (sortedInput ++ sortedOutput)

        [one] = filter (\w -> length w == 2) distinct
        [seven] = filter (\w -> length w == 3) distinct
        [four] = filter (\w -> length w == 4) distinct
        [eight] = filter (\w -> length w == 7) distinct

        twoThreeFive = filter (\w -> length w == 5) distinct
        sixNineZero = filter (\w -> length w == 6) distinct

        [six] = filter (\n -> (head one `elem` n) /= (last one `elem` n)) sixNineZero
        nineZero = filter (/= six) sixNineZero
        [nine] = filter (\n -> all (`elem` n) four) sixNineZero
        [zero] = filter (\n -> n /= nine && n /= six) sixNineZero

        [three] = filter (\n -> all (`elem`n) one) twoThreeFive

        [top] = filter (`notElem` one) seven
        [topRight] = filter (`notElem` six) letters
        [middle] = filter (`notElem` zero) letters
        [bottomLeft] = filter (`notElem` nine) letters
        [bottomRight] = filter (`elem` six) one
        [topLeft] = filter (`notElem` three) nine
        [bottom] = filter (`notElem` [top, topLeft, topRight, middle, bottomLeft, bottomRight]) letters
        mapping = [top, topLeft, topRight, middle, bottomLeft, bottomRight, bottom]

getInt :: [Char] -> [Char] -> Int
getInt [top, topLeft, topRight, middle, bottomLeft, bottomRight, bottom] n
    | L.sort [top, topLeft, topRight, bottomLeft, bottomRight, bottom] == n = 0 
    | length n == 2 = 1
    | L.sort [top, topRight, middle, bottomLeft, bottom] == n = 2
    | L.sort [top, topRight, middle, bottomRight, bottom] == n = 3
    | length n == 4 = 4
    | L.sort [top, topLeft, middle, bottomRight, bottom] == n = 5
    | L.sort [top, topLeft, middle, bottomLeft, bottomRight, bottom] == n = 6
    | length n == 3 = 7
    | length n == 7 = 8
    | L.sort [top, topLeft, topRight, middle, bottomRight, bottom] == n = 9
    | otherwise = error "uh oh"

getOutput :: String -> IO [([String], [String])]
getOutput path = do
    lines <- lines <$> readFile path
    let outputs = map parseLine lines
    return outputs

parseLine :: String -> ([String], [String])
parseLine l = (words inputs, words outputs)
    where
        [inputs, outputs] = splitOn "|" l
