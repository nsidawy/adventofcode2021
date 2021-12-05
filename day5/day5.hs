import Control.Monad
import Text.Printf
import Data.List.Split
import qualified Data.Map as M

type Point = (Int,Int)
type Coord = (Point,Point)

main = do  
    coords <- getData "input.txt"
    let straightCoords = filter isStraight coords
    let straightLinePoints = concat [uncurry getPoints c | c <- straightCoords]
    let allLinePoints = concat [uncurry getPoints c | c <- coords]
    let hotSpots1 = countHotSpots straightLinePoints
    let hotSpots2 = countHotSpots allLinePoints
    print hotSpots1
    print hotSpots2

countHotSpots :: [Point] -> Int
countHotSpots ps = length $ filter (\ (_, c) -> c > 1) $ M.toList $ M.fromListWith (+) [(p, 1) | p <- ps]

getPoints :: Point -> Point -> [Point]
getPoints (x1, y1) (x2, y2)
    | x1 == x2 && y1 == y2 = [(x1, y1)]
    | x1 == x2 && y1 < y2 = (x1, y1) : getPoints (x1, y1 + 1) (x2, y2) 
    | x1 == x2 && y1 > y2 = (x1, y1) : getPoints (x1, y1 - 1) (x2, y2) 
    | x1 < x2 && y1 == y2 = (x1, y1) : getPoints (x1 + 1, y1) (x2, y2) 
    | x1 > x2 && y1 == y2 = (x1, y1) : getPoints (x1 - 1, y1) (x2, y2) 
    | otherwise = (x1, y1) : getPoints (x1 + diagXInc , y1 + diagYInc) (x2, y2)
    where 
        diagXInc :: Int
        diagXInc = if x1 < x2 then 1 else -1 
        diagYInc :: Int
        diagYInc = if y1 < y2 then 1 else -1 

isStraight :: Coord -> Bool
isStraight ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

getData :: String -> IO [Coord]
getData path = do
    lines <- lines <$> readFile path
    let coordsStr = map (map (map read . splitOn "," ) . splitOn " -> ") lines
    return [((head $ head c, last $ head c), (head $ last c, last $ last c)) | c <- coordsStr]
