import           Control.Monad
import qualified Data.List       as L
import           Data.List.Split
import qualified Data.Set        as S
import qualified Data.Vector     as V
import           Text.Printf

type HeatMap = V.Vector (V.Vector Int)

main = do
    heatmap <- getOutput "input.txt"
    let lowpoints = getLowPoints heatmap [] 0 0
    print . sum $ map (\(v,_,_) -> v+1) lowpoints
    let basinSizes = map (\(_,x,y) -> fst $ getBasinSize heatmap S.empty x y 0) lowpoints
    print . product . take 3 . reverse . L.sort $ basinSizes 

getOutput :: String -> IO HeatMap
getOutput path = do
    lines <- lines <$> readFile path
    return $ V.fromList $ map parseLine lines
    where 
        parseLine l = V.fromList [read [c] | c <- l]

getLowPoints :: HeatMap -> [(Int, Int, Int)] -> Int -> Int -> [(Int, Int, Int)]
getLowPoints heatmap lowpoints x y
    | x == V.length (heatmap V.! 0) = lowpoints
    | y == V.length heatmap = getLowPoints heatmap lowpoints (x+1) 0
    | otherwise = getLowPoints heatmap newlowpoints x (y + 1)
    where
        current = getValue heatmap x y
        top = getValue heatmap x (y - 1)
        left = getValue heatmap (x - 1) y
        right = getValue heatmap (x + 1) y
        bottom = getValue heatmap x (y + 1)

        newlowpoints
            | current < minimum [top, left, right, bottom] = (current, x, y) : lowpoints
            | otherwise = lowpoints

getBasinSize :: HeatMap -> S.Set(Int,Int) -> Int -> Int -> Int -> (Int, S.Set (Int, Int))
getBasinSize heatmap visited x y size
    | getValue heatmap x y == 9
        || (x,y) `S.member` visited = (size, visited)
    | otherwise = (size'''', visited'''')
    where
        (size', visited') = getBasinSize heatmap ((x,y) `S.insert` visited) (x+1) y (size + 1)
        (size'', visited'') = getBasinSize heatmap visited' (x-1) y size'
        (size''', visited''') = getBasinSize heatmap visited'' x (y+1) size''
        (size'''', visited'''') = getBasinSize heatmap visited''' x (y-1) size'''

getValue :: HeatMap -> Int -> Int -> Int
getValue heatmap x y 
    | x < 0 || x >= V.length (heatmap V.! 0)
        || y < 0 || y >= V.length heatmap = 9
    | otherwise = (heatmap V.! y) V.! x
