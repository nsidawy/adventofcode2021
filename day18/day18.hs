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
    print $ printSnail final
    print $ getMagnitude final

    let maxOfPairs = maximum $ map getMagnitude $ concat [[reduce $ add x y, reduce $ add y x] | x <- snails, y <- snails, x /= y]
    print maxOfPairs

printSnail :: Snail -> String
printSnail (Value a) = show a
printSnail (Pair (a,b)) = "[" ++ printSnail a ++ "," ++ printSnail b ++ "]" 

getMagnitude :: Snail -> Int
getMagnitude (Value x) = x
getMagnitude (Pair (a,b)) = 3 * getMagnitude a + 2 * getMagnitude b 

add :: Snail -> Snail -> Snail
add s1 s2 = Pair (s1, s2)

reduce :: Snail -> Snail
reduce s 
    | M.isJust explodes = reduce $ explode s $ M.fromJust explodes
    | isSplit = reduce s'
    | otherwise = s
    where 
        explodes = hasExplode s 0
        (s',isSplit) = trySplit s

hasExplode :: Snail -> Int -> Maybe Snail
hasExplode (Value _) _ = M.Nothing
hasExplode p 4 = M.Just p
hasExplode (Pair (a,b)) i = if M.isNothing e1 then e2 else e1
    where 
        e1 = hasExplode a (i+1)
        e2 = hasExplode b (i+1)

explode :: Snail -> Snail -> Snail
explode s e = parseLine sStr'''
    where 
        sStr = printSnail s
        eStr = printSnail e
        (Pair (Value a, Value b)) = e
        eInd = getExplodeInd sStr 0 0
        
        addPrevious :: String -> Int -> Int -> String
        addPrevious s i v 
            | i < 0 = s
            | C.isDigit $ s !! i = take (i - length int + 1) s ++ show (v + read int) ++ drop (i+1) s
            | otherwise = addPrevious s (i-1) v
            where 
                int = lookbackInt s i
        sStr' = addPrevious sStr (eInd - 1) a

        addAfter :: String -> Int -> Int -> String
        addAfter s i v 
            | i >= length s = s
            | C.isDigit $ s !! i = take i s ++ show (v + read int) ++ drop (i + length int) s
            | otherwise = addAfter s (i+1) v
            where 
                int = lookforwardInt s i
        eInd' = getExplodeInd sStr' 0 0
        sStr'' = addAfter sStr' (eInd' + length eStr) b

        sStr''' = take eInd' sStr'' ++ "0" ++ drop (eInd' + length eStr) sStr''

getExplodeInd :: String -> Int -> Int -> Int
getExplodeInd [] _ _ = -1
getExplodeInd ('[':s) 4 i = i
getExplodeInd ('[':s) d i = getExplodeInd s (d+1) (i+1)
getExplodeInd (']':s) d i = getExplodeInd s (d-1) (i+1)
getExplodeInd (_:s) d i = getExplodeInd s d (i+1)

lookbackInt :: String -> Int  -> String
lookbackInt s i 
    | i < 0 = []
    | C.isDigit $ s !! i = lookbackInt s (i-1) ++ [s !! i]
    | otherwise = []

lookforwardInt :: String -> Int  -> String
lookforwardInt s i 
    | i >= length s = []
    | C.isDigit $ s !! i = s !! i : lookforwardInt s (i+1) 
    | otherwise = []

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
