import           Control.Monad
import           Data.List.Split
import qualified Data.Vector     as V

type FlashMap = V.Vector (V.Vector Int)

main = do
    flashMap <- getOutput "input.txt"
    print flashMap
    print $ step flashMap 100 0
    print $ findAllFlash flashMap 1

step :: FlashMap -> Int -> Int -> (FlashMap, Int)
step f 0 c = (f, c)
step f s c = step f'' (s-1) (c + c')
    where 
        fNext = increaseAll f 0 0
        (f', c') = processFlashes fNext []
        f'' = resetFlashes f' 0 0

findAllFlash :: FlashMap -> Int -> Int
findAllFlash f s = if c' == 100 then s else findAllFlash f'' (s+1)
    where 
        fNext = increaseAll f 0 0
        (f', c') = processFlashes fNext []
        f'' = resetFlashes f' 0 0

processFlashes :: FlashMap -> [(Int, Int)] -> (FlashMap, Int)
processFlashes f s = if length flashes == length flashes' then (f', length flashes) else processFlashes f' flashes
    where 
        flashes = getFlashes f 0 0 []
        f' = executeFlashes f (filter (`notElem` s) flashes)
        flashes' = getFlashes f' 0 0 []

executeFlashes :: FlashMap -> [(Int,Int)] -> FlashMap
executeFlashes f [] = f
executeFlashes f ((x,y):bs) = executeFlashes (increaseAdjacent f x y) bs

getFlashes :: FlashMap -> Int -> Int -> [(Int,Int)] -> [(Int, Int)]
getFlashes f x y b 
    | y == V.length f = b
    | x == V.length f = getFlashes f 0 (y + 1) b
    | otherwise = getFlashes f (x+1) y b'
    where 
        value = (f V.! y) V.! x 
        b' = if value == 10 then (x,y) : b else b

resetFlashes :: FlashMap -> Int -> Int -> FlashMap
resetFlashes f x y 
    | y == V.length f = f
    | x == V.length f = resetFlashes f 0 (y + 1)
    | otherwise = resetFlashes f' (x+1) y
    where 
        value = (f V.! y) V.! x 
        f' = if value == 10 then f V.// [(y, f V.! y V.// [(x, 0)])] else f

increaseAll :: FlashMap -> Int -> Int -> FlashMap
increaseAll f x y 
    | y == V.length f = f
    | x == V.length f = increaseAll f 0 (y + 1)
    | otherwise = increaseAll f' (x+1) y
    where 
        f' = increase f x y

increaseAdjacent :: FlashMap -> Int -> Int -> FlashMap
increaseAdjacent f x y = f''''''''
    where
        f' = increase f (x-1) (y-1)
        f'' = increase f' x (y-1)
        f''' = increase f'' (x+1) (y-1)
        f'''' = increase f''' (x-1) y
        f''''' = increase f'''' (x+1) y
        f'''''' = increase f''''' (x-1) (y+1)
        f''''''' = increase f'''''' x (y+1)
        f'''''''' = increase f''''''' (x+1) (y+1)


increase :: FlashMap -> Int -> Int -> FlashMap
increase f x y 
    | x < 0 || x >= V.length f || y < 0 || y >= V.length f = f
    | otherwise = f V.// [(y, f V.! y V.// [(x, newValue)])]
    where 
        value = (f V.! y) V.! x 
        newValue = if value == 10 then 10 else value + 1

getOutput :: String -> IO FlashMap
getOutput path = do
    lines <- lines <$> readFile path
    return $ V.fromList $ map parseLine lines
    where 
        parseLine l = V.fromList [read [c] | c <- l]
