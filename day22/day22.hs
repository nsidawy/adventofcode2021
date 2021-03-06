import           Control.Monad
import qualified Data.List       as L
import qualified Data.Map        as M
import qualified Data.Vector     as V
import Data.List.Split
import           Text.Printf

type Cube = M.Map (Int,Int,Int) Bool
type Range = ((Int,Int),(Int,Int),(Int,Int))
type Instruction = (Range,Bool)

main = do
    instructions <- getInput "input.txt"
    let cube = foldl applyLimited M.empty instructions
    let count = sum [if f then 1 else 0 | (_,f) <- M.toList cube]
    print count
    let result = foldl applyFull [] instructions
    print $ countRange result
    print "done"

applyLimited :: Cube -> Instruction -> Cube
applyLimited c (((x1,x2),(y1,y2),(z1,z2)),f) = foldl (\cube (x,y,z) -> M.insert (x,y,z) f cube) c coords 
    where 
        x1' = if x1 < -50 then -50 else x1
        x2' = if x2 > 50 then 50 else x2
        y1' = if y1 < -50 then -50 else y1
        y2' = if y2 > 50 then 50 else y2
        z1' = if z1 < -50 then -50 else z1
        z2' = if z2 > 50 then 50 else z2
        coords = [(x,y,z) | x <- [x1'..x2'], y <- [y1'..y2'], z <- [z1'..z2']]

applyFull :: [Range] -> Instruction -> [Range]
applyFull [] (r2,f) = [r2 | f]
applyFull (r1:rs) (r2,f) = r' ++ next
    where 
        r' = getRangeDiff r1 r2
        next = applyFull rs (r2,f)

getRangeDiff :: Range -> Range -> [Range]
getRangeDiff ((x11,x12),(y11,y12),(z11,z12)) ((x21,x22),(y21,y22),(z21,z22)) =
        [(x,y,z) | x <- xdo, y <- ydo, z <- zdo] ++
        [(x,y,z) | x <- xdo, y <- ydf, z <- zdf] ++
        [(x,y,z) | x <- xdf, y <- ydo, z <- zdf] ++
        [(x,y,z) | x <- xdf, y <- ydf, z <- zdo] ++
        [(x,y,z) | x <- xdo, y <- ydo, z <- zdf] ++
        [(x,y,z) | x <- xdf, y <- ydo, z <- zdo] ++
        [(x,y,z) | x <- xdo, y <- ydf, z <- zdo]
    where
        (xdo,xdf) = getDiff (x11,x12) (x21,x22)
        (ydo,ydf) = getDiff (y11,y12) (y21,y22)
        (zdo,zdf) = getDiff (z11,z12) (z21,z22)

getDiff :: (Int,Int) -> (Int,Int) -> ([(Int,Int)],[(Int,Int)])
getDiff (x11,x12) (x21,x22)
    | x11 >= x21 && x12 <= x22 = ([],[(x11,x12)])
    | x11 > x22 || x12 < x21 = ([(x11,x12)],[])
    | x11 >= x21 && x12 > x22 = ([(x22+1,x12)],[(x11,x22)])
    | x11 < x21 && x12 <= x22 = ([(x11,x21-1)],[(x21,x12)])
    | x11 < x21 && x12 > x22 = ([(x11,x21-1), (x22+1,x12)], [(x21,x22)])
    | otherwise = error $ show (x11,x12) ++ show (x21,x22)

countRange :: [Range] -> Int
countRange = foldl (\s ((x1,x2),(y1,y2),(z1,z2)) ->s + ((x2-x1)+1) * ((y2-y1)+1) * ((z2-z1)+1)) 0

getInput :: String -> IO [Instruction]
getInput path = do
    lines <- lines <$> readFile path
    return $ map parseLine lines

parseLine :: String -> Instruction
parseLine l = ((xRange, yRange, zRange), flip')
    where
        [flip,ranges] = words l
        flip' = flip == "on"
        [xRange,yRange,zRange] = map parseRange $ splitOn "," ranges

parseRange :: String -> (Int,Int)
parseRange r = (min,max)
    where
       [min,max] = map read $ splitOn ".." (drop 2 r) 
