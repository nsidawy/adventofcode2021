import           Control.Monad
import qualified Data.List       as L
import           Data.List.Split
import qualified Data.Map        as M
import qualified Data.Vector        as V
import           Data.Maybe
import           Text.Printf

main = do
    (start, transforms) <- getInput "input.txt"
    print (start, transforms)
    let part1 = runSteps start transforms 10
    print $ getCounts part1

    let countsByPair = M.fromList $ map (`getCountsByPair` transforms) $ M.keys transforms
    print "got counts by pair"
    let start' = V.fromList $ runSteps start transforms 20
    print $ V.length start'
    print "got step 20"
    let counts =  getCountsFromAggs (V.take 3000000 start') countsByPair M.empty 0
    let counts' = M.insertWith (+) (V.last start') 1 counts
    print counts'
    let min = minimum [c | (_, c) <- M.toList counts']
    let max = maximum [c | (_, c) <- M.toList counts']
    print (max - min)

getCountsByPair :: String -> M.Map String Char -> (String, M.Map Char Int)
getCountsByPair s m = (s, M.fromListWith (+) (zip (take (length step - 1) step) (repeat 1)))
    where
        step = runSteps s m 20

getCountsFromAggs :: V.Vector Char -> M.Map String (M.Map Char Int) -> M.Map Char Int -> Int -> M.Map Char Int
getCountsFromAggs cs m m0 i
    | i == V.length cs - 1 = m0
    | [c1,c2] `M.member` m = getCountsFromAggs cs m m0' (i+1)
    | otherwise = error "uh oh" -- c1 : step (c2:cs) m
    where
        counts = fromJust $ [c1,c2] `M.lookup` m
        c1 = cs V.! i
        c2 = cs V.! (i+1)
        m0' = foldl (\s (ch, c) -> M.insertWith (+) ch c s) m0 $ M.toList counts

getCounts :: String -> Int
getCounts s = max - min
    where
        count = M.fromListWith (+) $ zip s $ repeat 1
        min = minimum [c | (_, c) <- M.toList count]
        max = maximum [c | (_, c) <- M.toList count]

runSteps :: String -> M.Map String Char -> Int -> String
runSteps s m 0 = s
runSteps s m i = runSteps (step s m) m (i-1)

step :: String -> M.Map String Char -> String
step (c1:c2:cs) m
    | [c1,c2] `M.member` m = [c1, fromJust $ [c1,c2] `M.lookup` m] ++ step (c2:cs) m
    | otherwise = error "uh oh" -- c1 : step (c2:cs) m
step cs m = cs

getInput :: String -> IO (String, M.Map String Char)
getInput path = do
    lines <- lines <$> readFile path
    let start = head lines
    let transforms = M.fromList $ map parseTransform $ drop 2 lines
    return (start, transforms)

parseTransform :: String -> (String, Char)
parseTransform l = (a, head b)
    where
        [a,b] = splitOn " -> " l
