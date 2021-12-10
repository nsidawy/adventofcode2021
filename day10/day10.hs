import           Control.Monad
import qualified Data.List       as L
import           Data.List.Split
import qualified Data.Set        as S
import qualified Data.Vector     as V
import           Text.Printf

type HeatMap = V.Vector (V.Vector Int)

main = do
    lines <- lines <$> readFile "input.txt"
    let part1 = sum $ map (`processBad` []) lines
    let goodScores = L.sort $ filter (/=0) $map (`processGood` []) lines
    let part2 = goodScores !! (length goodScores `div` 2)
    print part1
    print part2

processBad :: [Char] -> [Char] -> Int
processBad [] _ = 0
processBad (c:cs) stack 
    | c `elem` ['(', '[', '{', '<'] = processBad cs (c:stack)
    | c == ')' && h == '('
        || c == ']' && h == '['
        || c == '}' && h == '{'
        || c == '>' && h == '<' 
            = processBad cs $ tail stack
    | otherwise = getBadScore c
    where
        h = head stack

processGood :: [Char] -> [Char] -> Int
processGood [] stack = getGoodScore stack 0
processGood (c:cs) stack
    | c `elem` ['(', '[', '{', '<'] = processGood cs (c:stack)
    | c == ')' && h == '('
        || c == ']' && h == '['
        || c == '}' && h == '{'
        || c == '>' && h == '<' 
            = processGood cs $ tail stack
    | otherwise = 0
    where
        h = head stack

getBadScore :: Char -> Int
getBadScore ')' = 3
getBadScore ']' = 57
getBadScore '}' = 1197
getBadScore '>' = 25137
getBadScore _ = error "oops"

getGoodScore :: [Char] -> Int -> Int
getGoodScore [] s = s
getGoodScore (c:cs) s = getGoodScore cs ((5 * s) + getCharScore c)
    where 
        getCharScore '(' = 1
        getCharScore '[' = 2
        getCharScore '{' = 3
        getCharScore '<' = 4
        getCharScore _ = error "oops"
