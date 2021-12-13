import           Control.Monad
import           Data.List.Split
import qualified Data.Char     as C

main = do
    connects <- getInput "input.txt"
    print $ length $ findPaths connects "start" [] False
    print $ length $ findPaths connects "start" [] True

findPaths :: [(String, String)] -> String -> [String] -> Bool -> [[String]]
findPaths _ "end" v _ = [v ++ ["end"]]
findPaths cs c v allowSmallRevisit
    | c == "start" && c `elem` v = []
    | c `elem` v && all C.isLower c && (not allowSmallRevisit || isSmallRevisit) = []
    | otherwise = concatMap (\x -> findPaths cs x v' allowSmallRevisit) nexts
    where 
        smalls = filter (all C.isLower) v
        isSmallRevisit = not $ null [x | x <- smalls, length (filter (== x) smalls) > 1]
        currents = filter (\(a,b) -> a == c || b == c) cs
        nexts = map (\(a,b) -> if a == c then b else a) currents
        v' = v ++ [c]

getInput :: String -> IO [(String,String)]
getInput path = do
    lines <- lines <$> readFile path
    return $ map parseLine lines

parseLine :: String -> (String,String)
parseLine l = (a,b)
    where
        [a,b] = splitOn "-" l
