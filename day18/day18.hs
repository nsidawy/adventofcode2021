import           Control.Monad
import qualified Data.List       as L
import qualified Data.Char       as C
import Text.Printf
import Data.Maybe as M
import Text.Read

data Snail =
    Value Int
    | Pair (Snail, Snail) deriving (Show, Eq)

main = do
    snails <- getInput "input.txt"
    let final = foldl1 (\s c -> reduce $ add s c) snails
    print $ snailToStr final
    print $ getMagnitude final

    let maxOfPairs = maximum $ map getMagnitude $ concat [[reduce $ add x y, reduce $ add y x] | x <- snails, y <- snails, x /= y]
    print maxOfPairs

getMagnitude :: Snail -> Int
getMagnitude (Value x) = x
getMagnitude (Pair (a,b)) = 3 * getMagnitude a + 2 * getMagnitude b 

add :: Snail -> Snail -> Snail
add s1 s2 = Pair (s1, s2)

reduce :: Snail -> Snail
reduce s 
    | isExplode = reduce sExplode
    | isSplit = reduce sSplit
    | otherwise = s
    where 
        (sExplode, isExplode) = tryExplode s
        (sSplit, isSplit) = trySplit s

trySplit :: Snail -> (Snail, Bool)
trySplit (Value x) 
    | x > 9 = (Pair (Value half, Value (half + remainder)), True)
    | otherwise = (Value x, False)
    where 
        half = x `div` 2
        remainder = x `mod` 2
trySplit (Pair (a,b))
    | isASplit = (Pair (a', b), True)
    | otherwise = (Pair (a', b'), isBSplit)
    where 
        (a', isASplit) = trySplit a
        (b', isBSplit) = trySplit b

tryExplode :: Snail -> (Snail, Bool)
tryExplode s
    | M.isNothing eInd = (s, False)
    | otherwise = (parseLine sStr''', True)
    where 
        sStr = snailToStr s
        eInd = getExplodeInd sStr 0 0
        eStr = getExplodeExp (drop (M.fromJust eInd) sStr)
        (Pair (Value a, Value b)) = parseLine eStr
        
        sStr' = addAfter sStr (M.fromJust eInd + length eStr) b
        sStr'' = addPrevious sStr' (M.fromJust eInd - 1) a
        eInd' = M.fromJust $ getExplodeInd sStr'' 0 0

        sStr''' = take eInd' sStr'' ++ "0" ++ drop (eInd' + length eStr) sStr''

getExplodeInd :: String -> Int -> Int -> Maybe Int
getExplodeInd [] _ _ = Nothing
getExplodeInd ('[':s) 4 i = Just i
getExplodeInd ('[':s) d i = getExplodeInd s (d+1) (i+1)
getExplodeInd (']':s) d i = getExplodeInd s (d-1) (i+1)
getExplodeInd (_:s) d i = getExplodeInd s d (i+1)

getExplodeExp :: String -> String
getExplodeExp (']':s) = [']']
getExplodeExp (c:s) = c : getExplodeExp s

addPrevious :: String -> Int -> Int -> String
addPrevious s i v 
    | i < 0 = s
    | C.isDigit $ s !! i = take (i - length int + 1) s ++ show (v + read int) ++ drop (i+1) s
    | otherwise = addPrevious s (i-1) v
    where 
        int = lookbackInt s i

lookbackInt :: String -> Int  -> String
lookbackInt s i 
    | i < 0 = []
    | C.isDigit c = lookbackInt s (i-1) ++ [c]
    | otherwise = []
    where
        c = s !! i

addAfter :: String -> Int -> Int -> String
addAfter s i v 
    | i >= length s = s
    | C.isDigit $ s !! i = take i s ++ show (v + read int) ++ drop (i + length int) s
    | otherwise = addAfter s (i+1) v
    where 
        int = lookforwardInt s i

lookforwardInt :: String -> Int  -> String
lookforwardInt s i 
    | i >= length s = []
    | C.isDigit c = c : lookforwardInt s (i+1) 
    | otherwise = []
    where
        c = s !! i

snailToStr :: Snail -> String
snailToStr (Value a) = show a
snailToStr (Pair (a,b)) = "[" ++ snailToStr a ++ "," ++ snailToStr b ++ "]" 

getInput :: String -> IO [Snail]
getInput path = do
    lines <- lines <$> readFile path
    return $ map parseLine lines

parseLine :: String -> Snail
parseLine s 
    | M.isJust (readMaybe s :: Maybe Int) = Value (read s) 
    | otherwise = Pair (parseLine a, parseLine b)
    where 
        strip = drop 1 $ take (length s - 1) s
        commaIndex = findComma strip 0 0
        a = take commaIndex strip
        b = drop (commaIndex+1) strip

findComma :: String -> Int -> Int -> Int
findComma (',':s) 0 i = i
findComma ('[':s) n i = findComma s (n+1) (i+1)
findComma (']':s) n i = findComma s (n-1) (i+1)
findComma (_:s) n i = findComma s n (i+1)
