import           Control.Monad
import qualified Data.List       as L
import           Data.List.Split
import           Data.Maybe
import qualified Data.Set        as S
import           Text.Printf

main = do
    (coords, folds) <- getInput "input.txt"
    let part1 = getFold coords (take 1 folds)
    print $ length part1

    let part2 = getFold coords folds
    let maxX = getMaxX part2
    let maxY = getMaxY part2
    printCoords (S.fromList part2) (0,0) maxX maxY

printCoords :: S.Set (Int,Int) -> (Int,Int) -> Int -> Int -> IO ()
printCoords coords (x,y) mx my
    | y > my = return ()
    | x > mx = do
        printf "\n"
        printCoords coords (0, y+1) mx my
    | otherwise = do
        if (x,y) `S.member` coords then printf "#" else printf "."
        printCoords coords (x+1, y) mx my

getFold :: [(Int, Int)] -> [(Char, Int)] -> [(Int, Int)]
getFold coords []            = L.nub coords
getFold coords (('x', i):fs) = getFold (getFoldX coords i) fs
getFold coords (('y', i):fs) = getFold (getFoldY coords i) fs

getFoldX :: [(Int,Int)] -> Int -> [(Int,Int)]
getFoldX coords i = [if x < i then (x,y) else (maxX - x, y) | (x,y) <- coords]
    where
        maxX = getMaxX coords

getFoldY :: [(Int,Int)] -> Int -> [(Int,Int)]
getFoldY coords i = [if y < i then (x,y) else (x, maxY - y) | (x,y) <- coords]
    where
        maxY = getMaxY coords

getMaxX :: [(Int,Int)] -> Int
getMaxX coords = maximum [x | (x,_) <- coords]

getMaxY :: [(Int,Int)] -> Int
getMaxY coords = maximum [y | (_,y) <- coords]

getInput :: String -> IO ([(Int,Int)], [(Char,Int)])
getInput path = do
    lines <- lines <$> readFile path
    let break = fromJust $ L.elemIndex "" lines
    let coords = map parseCoords (take break lines)
    let folds = map parseFolds (drop (break+1) lines)
    return (coords, folds)

parseCoords :: String -> (Int,Int)
parseCoords l = (a,b)
    where
        [a,b] = map read $ splitOn "," l

parseFolds :: String -> (Char,Int)
parseFolds l = (head a,read b)
    where
        [a,b] = splitOn "=" (words l !! 2)
