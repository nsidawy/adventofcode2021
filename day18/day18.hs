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
    | M.isJust explodePair = reduce sExplode''
    | isSplit = reduce sSplit
    | otherwise = s
    where 
        (sExplode, explodePair, path) = tryExplode s 0 ""
        (sSplit, isSplit) = trySplit s

        Pair (Value a, Value b) = M.fromJust explodePair
        path' = M.fromJust path

        lastLeftInd = L.elemIndex 'L' (reverse path')
        sExplode'
            | M.isJust lastLeftInd = addToPath sExplode (take (length path' - M.fromJust lastLeftInd - 1) path' ++ "R" ++ repeat 'L') b 
            | otherwise = sExplode

        lastRightInd = L.elemIndex 'R' (reverse path')
        sExplode''
            | M.isJust lastRightInd = addToPath sExplode' (take (length path' - M.fromJust lastRightInd - 1) path' ++ "L" ++ repeat 'R') a 
            | otherwise = sExplode'

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

tryExplode :: Snail -> Int -> String -> (Snail, Maybe Snail, Maybe String)
tryExplode (Value x) _ _ = (Value x, Nothing, Nothing)
tryExplode (Pair (a, b)) 4 p = (Value 0, Just $ Pair (a, b), Just p)  
tryExplode (Pair (a, b)) d p 
    | M.isJust as = (Pair (a', b), as, ap)
    | otherwise = (Pair (a', b'), bs , bp)
    where
        (a', as, ap) = tryExplode a (d+1) (p++"L")
        (b', bs, bp) = tryExplode b (d+1) (p++"R")

addToPath :: Snail -> String -> Int -> Snail
addToPath (Value x) _ i = Value (x+i)
addToPath (Pair (a,b)) ('L':p) i = Pair (addToPath a p i, b)
addToPath (Pair (a,b)) ('R':p) i = Pair (a, addToPath b p i)

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
