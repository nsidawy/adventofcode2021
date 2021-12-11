import           Control.Monad
import           Control.Monad.State
import           Data.List.Split
import qualified Data.Vector     as V

type FlashMap = V.Vector (V.Vector Int)

main = do
    flashMap <- getInput "input.txt"
    print $ step flashMap 100
    print $ findAllFlash flashMap 1

step :: FlashMap -> Int -> Int
step _ 0 = 0
step f s = c + step f'' (s-1)
    where 
        fNext = increaseAll f
        (f', c) = processFlashes fNext []
        f'' = resetFlashes f'

findAllFlash :: FlashMap -> Int -> Int
findAllFlash f s 
    | c' == 100 = s 
    | otherwise = findAllFlash f'' (s+1)
    where 
        fNext = increaseAll f
        (f', c') = processFlashes fNext []
        f'' = resetFlashes f'

processFlashes :: FlashMap -> [(Int, Int)] -> (FlashMap, Int)
processFlashes f s 
    | length flashes == length flashes' = (f', length flashes)
    | otherwise = processFlashes f' flashes
    where 
        flashes = getFlashes f
        f' = executeFlashes f (filter (`notElem` s) flashes)
        flashes' = getFlashes f'

executeFlashes :: FlashMap -> [(Int,Int)] -> FlashMap
executeFlashes f [] = f
executeFlashes f ((x,y):bs) = executeFlashes (increaseAdjacent f x y) bs

getFlashes :: FlashMap  -> [(Int, Int)]
getFlashes f = [(x, y) 
        | x <- [0..d]
        , y <- [0..d]
        , (f V.! y) V.! x == 10]
    where
        d = V.length f - 1

resetFlashes :: FlashMap -> FlashMap
resetFlashes = V.map (V.map (\x -> if x == 10 then 0 else x))

increaseAll :: FlashMap -> FlashMap
increaseAll = V.map (V.map (\x -> if x == 10 then 10 else x + 1))

increaseAdjacent :: FlashMap -> Int -> Int -> FlashMap
increaseAdjacent f x y = flip execState f $ do
    modify $ \f' -> increase f' (x-1) (y-1)
    modify $ \f' -> increase f' x (y-1)
    modify $ \f' -> increase f' (x+1) (y-1)
    modify $ \f' -> increase f' (x-1) y
    modify $ \f' -> increase f' (x+1) y
    modify $ \f' -> increase f' (x-1) (y+1)
    modify $ \f' -> increase f' x (y+1)
    modify $ \f' -> increase f' (x+1) (y+1)

increase :: FlashMap -> Int -> Int -> FlashMap
increase f x y 
    | x < 0 || x >= V.length f || y < 0 || y >= V.length f = f
    | otherwise = f V.// [(y, f V.! y V.// [(x, newValue)])]
    where 
        value = (f V.! y) V.! x 
        newValue = if value == 10 then 10 else value + 1

getInput :: String -> IO FlashMap
getInput path = do
    lines <- lines <$> readFile path
    return $ V.fromList $ map parseLine lines
    where 
        parseLine l = V.fromList [read [c] | c <- l]
