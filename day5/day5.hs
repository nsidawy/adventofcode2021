import Control.Monad
import Text.Printf
import Data.List.Split
import Data.Sort
import qualified Data.Vector as V

type Point = (Int,Int)
type Coord = (Point,Point)

main = do  
    coords <- getData "input.txt"
    let straightCoords = filter isStraight coords
    let maxX = maximum $ concatMap (\i -> [fst $ fst i, fst $ snd i]) coords
    let maxY = maximum $ concatMap (\i -> [snd $ fst i, snd $ snd i]) coords
    let points = [(x,y) | x <- [0..maxX] , y <- [0..maxY]]
    let hotSpots1 = [p | p <- points, countIntersects straightCoords p >= 2]
    print (length hotSpots1)
    let hotSpots2 = [p | p <- points, countIntersects coords p >= 2]
    print (length hotSpots2)

countIntersects :: [Coord] -> Point -> Int
countIntersects cs p = length (filter (intersects p) cs)

intersects :: Point -> Coord -> Bool
intersects p c = straightIntersects p c && (isStraight c || diffX == diffY)
    where
        diffX :: Int
        diffX = abs $ fst p - firstX c
        diffY :: Int
        diffY = abs $ snd p - firstY c

straightIntersects :: Point -> Coord -> Bool
straightIntersects p c = ((fst p >= firstX c && fst p <= sndX c) || (fst p >= sndX c && fst p <= firstX c))
        && ((snd p >= firstY c && snd p <= sndY c) || (snd p >= sndY c && snd p <= firstY c))

diagonalIntersects :: Point -> Coord -> Bool
diagonalIntersects p c = False

getPoints :: Coord -> [Point]
getPoints c = []
        
isStraight :: Coord -> Bool
isStraight c = firstX c == sndX c || firstY c == sndY c

firstX :: Coord -> Int
firstX c = fst $ fst c

sndX :: Coord -> Int
sndX c = fst $ snd c

firstY :: Coord -> Int
firstY c = snd $ fst c

sndY :: Coord -> Int
sndY c = snd $ snd c

getData :: String -> IO [Coord]
getData path = do
    lines <- lines <$> readFile path
    let coordsStr = map (\l -> map (map read . splitOn "," ) $ splitOn " -> " l) lines
    let coords = [((head $ head x, last $ head x), (head $ last x, last $ last x)) | x <- coordsStr]
    return coords
