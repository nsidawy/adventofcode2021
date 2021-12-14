import           Control.Monad
import qualified Data.List       as L
import           Data.List.Split
import qualified Data.Map        as M
import Data.Maybe
import           Text.Printf

main = do
    (start, transforms) <- getInput "input.txt"
    print (start, transforms)
    let part1 = runSteps start transforms 10
    print $ getCounts part1

    let aggs = M.fromList $ map (\s -> (s, runSteps s transforms 20)) $ M.keys transforms
    let countsByPair = M.fromList $ map (\(s, s2) -> (s, M.fromListWith (+) $ zip (take (length s2 - 1) s2) $ repeat 1)) $ M.toList aggs
    print "got counts by pair"
    let start' = runSteps start transforms 20
    print "got step 20"
    let counts =  getCountsFromAggs start' countsByPair
    let counts' = M.insertWith (+) (last start') 1 counts
    print counts'
    let min = minimum [c | (_, c) <- M.toList counts']
    let max = maximum [c | (_, c) <- M.toList counts']
    print (max - min)

    --let resultFromAggs = getResultsFromAggs start aggs
    --print $ getCounts resultFromAggs
    --let resultFromAggs2 = getResultsFromAggs resultFromAggs aggs
    --print $ getCounts resultFromAggs2
    --part2 <- runSteps start transforms 30
    --print $ length part2
    --print $ getCounts part2

getResultsFromAggs :: String -> M.Map String String -> String
getResultsFromAggs (c1:c2:cs) m 
    | [c1,c2] `M.member` m = take (length r - 1) r ++ getResultsFromAggs (c2:cs) m
    | otherwise = error "uh oh" -- c1 : step (c2:cs) m
    where 
        r = fromJust $ [c1,c2] `M.lookup` m
getResultsFromAggs cs m = cs

getCountsFromAggs :: String -> M.Map String (M.Map Char Int) -> M.Map Char Int
getCountsFromAggs (c1:c2:cs) m 
    | [c1,c2] `M.member` m = foldl (\s (ch, c) -> M.insertWith (+) ch c s) counts $ M.toList next
    | otherwise = error "uh oh" -- c1 : step (c2:cs) m
    where 
        counts = fromJust $ [c1,c2] `M.lookup` m
        next = getCountsFromAggs (c2:cs) m
getCountsFromAggs cs m = M.empty

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
