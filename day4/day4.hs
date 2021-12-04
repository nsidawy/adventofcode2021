{-# LANGUAGE ParallelListComp #-}

import Control.Monad
import Text.Printf
import Data.List.Split
import qualified Data.Vector as V

type Board = V.Vector (V.Vector Int)
type Row = V.Vector Int

main = do  
    (numbers, boards) <- getData "input.txt"
    let winners = getWinners numbers 1 boards []
    print winners
    printf "Part 1: %d\n" $ scoreBoard numbers $ head winners
    printf "Part 2: %d\n" $ scoreBoard numbers $ last winners

getData :: String -> IO ([Int], [Board])
getData path = do
    lines <- lines <$> readFile path
    let numbers = map read $ splitOn "," $ head lines
    let boards = getBoards (tail lines) []
    return (numbers, boards)

getWinners :: [Int] -> Int -> [Board] -> [(Int, Board)] -> [(Int, Board)]
getWinners ns i boards winners = if length boards == 0 then winners else getWinners ns (i+1) losers (winners ++ newWinners)
    where
        called :: [Int]
        called = take i ns

        losers :: [Board]
        losers = [b | b <- boards, not $ checkBoard called b]
        
        newWinners :: [(Int, Board)]
        newWinners = zip (repeat i) [b | b <- boards, checkBoard called b]

getBoards :: [String] -> [Board] -> [Board]
getBoards [] v = v
getBoards ls v = getBoards (drop 6 ls) (newBoard : v)
    where 
        boardLines :: [String]
        boardLines = take 5 $ tail ls 

        newBoard :: Board
        newBoard = V.fromList [V.fromList ((map read $ words bl) :: [Int]) | bl <- boardLines] 

scoreBoard :: [Int] -> (Int, Board) -> Int
scoreBoard ns (i, board) = (ns !! (i-1)) * (sum [v | v <- flattened, not $ v `elem` called])
    where
        flattened :: [Int]
        flattened = V.toList $ V.concat $ V.toList board

        called :: [Int]
        called = i `take` ns
        

checkBoard :: [Int] -> Board -> Bool
checkBoard ns board = validRows || validCols
    where 
        validRows :: Bool
        validRows = V.any (\r -> checkRow ns r) board

        validCols :: Bool
        validCols = any (\i -> checkColumn ns board i) [0..4]

checkRow :: [Int] -> Row -> Bool 
checkRow ns row = V.all (\v -> elem v ns) row

checkColumn :: [Int] -> Board -> Int -> Bool
checkColumn ns board i = all (\v -> elem v ns) (V.map (\r -> r V.! i) board)
