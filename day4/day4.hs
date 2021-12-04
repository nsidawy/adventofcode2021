{-# LANGUAGE ParallelListComp #-}

import Control.Monad
import Text.Printf
import Data.List.Split
import Data.Sort
import qualified Data.Vector as V

type Board = V.Vector (V.Vector Int)
type Row = V.Vector Int

main = do  
    (numbers, boards) <- getData "input.txt"
    let winningTurns = sortOn (\v -> snd v) [(b, getWinningTurn numbers 1 b) | b <- boards]
    let (firstBoard, firstTurn) = head winningTurns
    let (lastBoard, lastTurn) = last winningTurns
    printf "Part 1: %d\n" $ scoreBoard (firstTurn `take` numbers) firstBoard
    printf "Part 2: %d\n" $ scoreBoard (lastTurn `take` numbers) lastBoard

getData :: String -> IO ([Int], [Board])
getData path = do
    lines <- lines <$> readFile path
    let numbers = map read $ splitOn "," $ head lines
    let boards = getBoards (tail lines) []
    return (numbers, boards)

getWinningTurn :: [Int] -> Int -> Board -> Int
getWinningTurn ns i board = if checkBoard (take i ns) board then i else getWinningTurn ns (i+1) board

getBoards :: [String] -> [Board] -> [Board]
getBoards [] v = v
getBoards ls v = getBoards (drop 6 ls) (newBoard : v)
    where 
        boardLines :: [String]
        boardLines = take 5 $ tail ls 

        newBoard :: Board
        newBoard = V.fromList [V.fromList ((map read $ words bl) :: [Int]) | bl <- boardLines] 

scoreBoard :: [Int] -> Board -> Int
scoreBoard ns board = (last ns) * (sum [v | v <- flattened, not $ v `elem` ns])
    where
        flattened :: [Int]
        flattened = V.toList $ V.concat $ V.toList board

checkBoard :: [Int] -> Board -> Bool
checkBoard ns board = validRows || validCols
    where 
        checkRow :: [Int] -> Row -> Bool 
        checkRow ns row = V.all (\v -> elem v ns) row
        
        checkColumn :: [Int] -> Board -> Int -> Bool
        checkColumn ns board i = all (\v -> elem v ns) (V.map (\r -> r V.! i) board)

        validRows :: Bool
        validRows = V.any (\r -> checkRow ns r) board

        validCols :: Bool
        validCols = any (\i -> checkColumn ns board i) [0..4]

