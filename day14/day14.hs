import           Control.Monad
import qualified Data.List       as L
import           Data.List.Split
import qualified Data.Map        as M
import qualified Data.Vector        as V
import           Data.Maybe
import           Text.Printf

main = do
    (start, transforms) <- getInput "input.txt"
    let part1 = runSteps start transforms 10
    print $ getResult $ getCounts part1

    let start' = runSteps start transforms 20
    let pairCounts = L.foldl' (\s (p1,p2) -> M.insertWith (+) [p1,p2] 1 s) M.empty (zip start' $ tail start')
    let charCountsByPair = map (`getCharCountsByPair` transforms) $ M.keys pairCounts
    let countsByPair = [M.fromList [(c, ct' * pairCounts M.! p) | (c,ct') <- M.toList ct] | (p, ct) <- charCountsByPair]
    let counts = L.foldl' (M.unionWith (+)) M.empty countsByPair
    let final = M.insertWith (+) (last start') 1 counts
    print $ getResult final

getCharCountsByPair :: String -> M.Map String Char -> (String, M.Map Char Int)
getCharCountsByPair s m = (s, getCounts (take (length step - 1) step))
    where
        step = runSteps s m 20

getCounts :: String -> M.Map Char Int
getCounts s = M.fromListWith (+) $ zip s $ repeat 1

getResult :: M.Map Char Int -> Int
getResult m = max - min
    where
        min = minimum [c | (_, c) <- M.toList m]
        max = maximum [c | (_, c) <- M.toList m]

runSteps :: String -> M.Map String Char -> Int -> String
runSteps s m 0 = s
runSteps s m i = runSteps (step s m) m (i-1)

step :: String -> M.Map String Char -> String
step (c1:c2:cs) m
    | [c1,c2] `M.member` m = [c1, fromJust $ [c1,c2] `M.lookup` m] ++ step (c2:cs) m
    | otherwise = error "uh oh"
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
