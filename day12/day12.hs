import           Control.Monad
import           Data.List.Split
import qualified Data.Char     as C

main = do
    connects <- getInput "input.txt"
    print connects
    let paths = findPaths connects "start" []
    print $ length paths
    paths2 <- findPaths2 connects "start" []
    print $ length paths2

findPaths :: [(String, String)] -> String -> [String] -> [[String]]
findPaths _ "end" v = ["end" : v]
findPaths cs c v
    | c `elem` v && all C.isLower c = []
    | otherwise = concatMap (\x -> findPaths cs x v') nexts
    where
        currents = filter (\(a,b) -> a == c || b == c) cs
        nexts = map (\(a,b) -> if a == c then b else a) currents
        v' = c:v
        

findPaths2 :: [(String, String)] -> String -> [String] -> IO [[String]]
findPaths2 _ "end" v = return ["end" : v]
findPaths2 cs c v
    | c == "start" && c `elem` v = return []
    | c `elem` v && C.isLower (head c) && is2Smalls = return []
    | otherwise = do
        let currents = filter (\(a,b) -> a == c || b == c) cs
        let nexts = map (\(a,b) -> if a == c then b else a) currents
        let v' = c:v
        paths <- mapM (\x -> findPaths2 cs x v') nexts
        return $ concat paths 
    where 
        smalls = filter (all C.isLower) v
        is2Smalls = not $ null  [x | x <- smalls, length (filter (== x) smalls) > 1]

getInput :: String -> IO [(String,String)]
getInput path = do
    lines <- lines <$> readFile path
    return $ map parseLine lines
    where 
        parseLine l = (a,b)
            where
                [a,b] = splitOn "-" l
