import           Control.Monad
import           Data.List.Split
import           Text.Printf
import qualified Data.Vector as V
import qualified Data.List as L

main = do
    heatmap <- getOutput "input.txt"
    let lowpoints = getLowPoints heatmap [] 0 0
    print $ sum $ map (\(v,_,_) -> v+1) lowpoints
    print $ product . take 3 . reverse . L.sort $ map (\(_,x,y) -> fst $ getBasinSize heatmap [] x y 0) lowpoints

getOutput :: String -> IO (V.Vector (V.Vector Int))
getOutput path = do
    lines <- lines <$> readFile path
    return $ V.fromList $ map parseLine lines

getLowPoints :: V.Vector (V.Vector Int) -> [(Int, Int, Int)] -> Int -> Int -> [(Int, Int, Int)]
getLowPoints heatmap lowpoints x y
    | x == V.length (heatmap V.! 0) = lowpoints
    | y == V.length heatmap = getLowPoints heatmap lowpoints (x+1) 0
    | otherwise = getLowPoints heatmap newlowpoints x (y + 1)
    where
        getValue a b = (heatmap V.! b) V.! a
        current = getValue x y
        top = if y == 0 then 1000000 else getValue x (y - 1)
        left = if x == 0 then 1000000 else getValue (x - 1) y
        right = if x + 1 == V.length (heatmap V.! 0) then 1000000 else getValue (x + 1) y
        bottom = if y + 1 == V.length heatmap then 1000000 else getValue x (y + 1)

        newlowpoints = if current < minimum [top, left, right, bottom] then (current, x, y) : lowpoints else lowpoints

getBasinSize :: V.Vector (V.Vector Int) -> [(Int,Int)] -> Int -> Int -> Int -> (Int, [(Int, Int)])
getBasinSize heatmap visited x y size
    | x < 0 || x >= V.length (heatmap V.! 0)
        || y < 0 || y >= V.length heatmap 
        || getValue x y == 9
        || (x,y) `elem` visited = (size, visited)
    | otherwise = (size'''', visited'''')
    where
        getValue a b = (heatmap V.! b) V.! a
        (size', visited') = getBasinSize heatmap ((x,y) : visited) (x+1) y (size + 1)
        (size'', visited'') = getBasinSize heatmap visited' (x-1) y size'
        (size''', visited''') = getBasinSize heatmap visited'' x (y+1) size''
        (size'''', visited'''') = getBasinSize heatmap visited''' x (y-1) size'''

parseLine :: String -> V.Vector Int
parseLine l = V.fromList [read [c] | c <- l]
