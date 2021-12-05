import Control.Monad
import Text.Printf
import Data.List.Split
import Data.Sort
import qualified Data.Vector as V

type Point = (Int,Int)
type Coord = (Point,Point)

main = do  
    coords <- getData "ex.txt"
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
intersects p c = bounded p c && (isStraight c || diffX == diffY)
    where
        diffX :: Int
        diffX = abs $ fst p - fst (fst c)
        diffY :: Int
        diffY = abs $ snd p - snd (fst c)

bounded :: Point -> Coord -> Bool
bounded p c = (fst p >= minX c && fst p <= maxX c)
        && (snd p >= minY c && snd p <= maxY c)

getPoints :: Coord -> [Point]
getPoints c = []
        
isStraight :: Coord -> Bool
isStraight c = fst (fst c) == fst (snd c) || snd (fst c) == snd (snd c)

minX :: Coord -> Int
minX c 
    | fst (fst c) < fst (snd c) = fst $ fst c
    | otherwise = fst $ snd c

maxX :: Coord -> Int
maxX c 
    | fst (fst c) < fst (snd c) = fst $ snd c
    | otherwise = fst $ fst c

minY :: Coord -> Int
minY c 
    | snd (fst c) < snd (snd c) = snd $ fst c
    | otherwise = snd $ snd c

maxY :: Coord -> Int
maxY c 
    | snd (fst c) < snd (snd c) = snd $ snd c
    | otherwise = snd $ fst c

getData :: String -> IO [Coord]
getData path = do
    lines <- lines <$> readFile path
    let coordsStr = map (\l -> map (map read . splitOn "," ) $ splitOn " -> " l) lines
    let coords = [((head $ head x, last $ head x), (head $ last x, last $ last x)) | x <- coordsStr]
    return coords
