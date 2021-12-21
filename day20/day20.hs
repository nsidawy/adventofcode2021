import           Control.Monad
import qualified Data.List       as L
import qualified Data.Map        as M
import qualified Data.Vector     as V
import           Text.Printf

type Codex = V.Vector Char
type Grid = M.Map (Int,Int) Char

main = do
    (codex, gridMap) <- getInput "input.txt"
    printGrid gridMap
    let result = solve codex gridMap 50
    let count = sum [if v == '#' then 1 else 0 | (_,v) <- M.toList result]
    printGrid result
    print count

solve :: Codex -> Grid -> Int -> Grid
solve _ g 0 = g
solve c g n = solve c g' (n-1)
    where
        xs = [x | (x,_) <- M.keys g]
        ys = [y | (_,y) <- M.keys g]
        minX = minimum xs - 1
        maxX = maximum xs + 1
        minY = minimum ys - 1
        maxY = maximum ys + 1
        g' = M.fromList $ step c g (minX,maxX,minY,maxY) (minX,minY) (odd n)

step :: Codex -> Grid -> (Int,Int,Int,Int) -> (Int,Int) -> Bool -> [((Int,Int),Char)]
step c g (minX,maxX,minY,maxY) (x,y) isOdd
    | y > maxY = []
    | x > maxX = step c g (minX,maxX,minY,maxY) (minX, y+1) isOdd
    | otherwise = ((x,y),v) : step c g (minX,maxX,minY,maxY) (x+1, y) isOdd
        where
            def = if isOdd && c V.! 0 == '#' then '#' else '.'
            get c = M.findWithDefault def c g
            bs = [get (x-1,y-1), get (x,y-1), get (x+1,y-1),
                get (x-1,y), get (x,y), get (x+1,y),
                get (x-1,y+1), get (x,y+1), get (x+1,y+1)]
            v = c V.! binToDec bs

printGrid :: Grid -> IO ()
printGrid g = printGrid' g (minX,maxX,minY,maxY) (minX,minY)
    where
        xs = [x | (x,_) <- M.keys g]
        ys = [y | (_,y) <- M.keys g]
        minX = minimum xs
        maxX = maximum xs
        minY = minimum ys
        maxY = maximum ys

printGrid' :: Grid -> (Int,Int,Int,Int) -> (Int,Int) -> IO ()
printGrid' g (minX,maxX,minY,maxY) (x,y)
    | y > maxY = printf "\n"
    | x > maxX = do
        printf "\n"
        printGrid' g (minX,maxX,minY,maxY) (minX, y+1)
    | otherwise = do
        printf "%c" $ g M.! (x,y)
        printGrid' g (minX,maxX,minY,maxY) (x+1, y)

binToDec :: [Char] -> Int
binToDec bs = sum [b * 2^i | (b,i) <- zip bs' [0..]]
    where
        bs' = reverse $ map (\c -> if c == '#' then 1 else 0) bs

getInput :: String -> IO (Codex, Grid)
getInput path = do
    lines <- lines <$> readFile path
    let codex = V.fromList $ head lines
    let gridVector = V.fromList $ map V.fromList $ drop 2 lines
    let gridMap = M.fromList [((x,y), i) |
            y <- [0..V.length gridVector - 1],
            x <- [0..V.length (gridVector V.! 0) - 1],
            let i = gridVector V.! y V.! x]
    return (codex, gridMap)
