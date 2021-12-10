import           Control.Monad
import qualified Data.List       as L
import           Data.List.Split
import qualified Data.Set        as S
import qualified Data.Vector     as V
import           Text.Printf

type HeatMap = V.Vector (V.Vector Int)

main = do
    lines <- getOutput "input.txt"
    let scores = sum $ map (`processBad` []) lines
    let goodScores = L.sort $ filter (/=0) $map (`processGood` []) lines
    let final = goodScores !! (length goodScores `div` 2)
    print scores
    print goodScores
    print final

processBad :: [Char] -> [Char] -> Int
processBad [] _ = 0
processBad (c:cs) stack = if c `elem` ['(', '[', '{', '<'] 
    then processBad cs (c:stack)
    else if c == ')' && h == '(' || c == ']' && h == '[' || c == '}' && h == '{' || c == '>' && h == '<'
        then processBad cs $ tail stack
        else getBadScore c
    where
        h = head stack

processGood :: [Char] -> [Char] -> Int
processGood [] stack = getGoodScore stack 0
processGood (c:cs) stack = if c `elem` ['(', '[', '{', '<'] 
    then processGood cs (c:stack)
    else if c == ')' && h == '(' || c == ']' && h == '[' || c == '}' && h == '{' || c == '>' && h == '<'
        then processGood cs $ tail stack
        else 0
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
getGoodScore ('(':cs) s = getGoodScore cs ((5 * s) + 1)
getGoodScore ('[':cs) s = getGoodScore cs ((5 * s) + 2)
getGoodScore ('{':cs) s = getGoodScore cs ((5 * s) + 3)
getGoodScore ('<':cs) s = getGoodScore cs ((5 * s) + 4)
getGoodScore _ _ = error "oops"

getOutput :: String -> IO [String]
getOutput path = lines <$> readFile path
