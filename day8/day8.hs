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
process (inputs, outputs) = 1000 * getInt mapping (head sortedOutput) + 100 * getInt mapping (sortedOutput !! 1) + 10 * getInt mapping (sortedOutput !! 2) + getInt mapping (last sortedOutput)
    where 
        letters = ['a', 'b', 'c', 'd', 'e', 'f', 'g']
        -- sort inputs and ouputs so we have one string per digit
        sortedInput = map L.sort inputs
        sortedOutput = map L.sort outputs
        distinct = L.nub (sortedInput ++ sortedOutput)
        [one] = [w | w <- distinct, length w == 2]
        [seven] = [w | w <- distinct, length w == 3]
        [four] = [w | w <- distinct, length w == 4]
        [eight] = [w | w <- distinct, length w == 7]
        sixNineZero = [w | w <- distinct, length w == 6]
        [six] = [n | n <- sixNineZero, (head one `elem` n) /= (last one `elem` n)]
        nineZero = [n | n <- sixNineZero, n /= six]
        [nine] = [n | n <- nineZero, head four `elem` n && four !! 1 `elem` n && four !! 2 `elem` n && four !! 3 `elem` n]
        twoThreeFive = [w | w <- distinct, length w == 5]
        [three] = filter (\x -> head one `elem` x && last one `elem` x) twoThreeFive
        [zero] = filter (/= nine) nineZero
        [top] = [c | c <- seven, c `notElem` one]
        [topRight] = filter (`notElem` six) letters
        [middle] = filter (`notElem` zero) letters
        [bottomLeft] = filter (`notElem` nine) letters
        [bottomRight] = [c | c <- one, c `elem` six]
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
