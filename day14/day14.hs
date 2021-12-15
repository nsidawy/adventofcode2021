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

    let countsByPair = M.fromList $ map (`getCountsByPair` transforms) $ M.keys transforms
    let start' = V.fromList $ runSteps start transforms 20
    let pairs = [[start' V.! i, start' V.! (i+1)] | i <- [0..V.length start' -2]] 
    let pairCounts = [fromJust $ M.lookup p countsByPair | p <- pairs]
    --let counts = foldl (M.unionWith (+)) M.empty (take 4000000 pairCounts)
    --let counts' = foldl (M.unionWith (+)) counts (take 4000000 $ drop 4000000  pairCounts)
    --let counts'' = foldl (M.unionWith (+)) counts' (take 4000000 $ drop 8000000  pairCounts)
    --let counts''' = foldl (M.unionWith (+)) counts'' (take 4000000 $ drop 12000000  pairCounts)
    --let counts'''' = foldl (M.unionWith (+)) counts''' (take 4000000 $ drop 16000000  pairCounts)
    --let final = M.insertWith (+) (V.last start') 1 counts''''
    --let counts = chunkUnionWith pairCounts M.empty 4000000
    let counts = L.foldl' (M.unionWith (+)) M.empty pairCounts
    let final = M.insertWith (+) (V.last start') 1 counts
    print $ getResult final

chunkUnionWith :: [M.Map Char Int] -> M.Map Char Int -> Int -> M.Map Char Int
chunkUnionWith [] m _ = m
chunkUnionWith ms m n = chunkUnionWith (drop n ms) m' n
    where
        m' = foldl (M.unionWith (+)) m (take n ms)

getCountsByPair :: String -> M.Map String Char -> (String, M.Map Char Int)
getCountsByPair s m = (s, M.fromListWith (+) (zip (take (length step - 1) step) (repeat 1)))
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
