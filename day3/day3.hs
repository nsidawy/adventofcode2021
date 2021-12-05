{-# LANGUAGE ParallelListComp #-}

import Control.Monad
import Text.Printf

main = do  
    lines <- getLines "input.txt"
    let counts = replicate (length $ head lines) 0
    let gammaBinary = getGammaBinary lines counts
    let epsilonBinary = getEpsilonBinary gammaBinary
    printf "part 1: %d\n" $ binaryToInt gammaBinary * binaryToInt epsilonBinary
    let oxygenBinary = getOxygenBinary lines 0
    let co2Binary = getCo2Binary lines 0
    printf "part 2: %d\n" $ binaryToInt co2Binary * binaryToInt oxygenBinary

getOxygenBinary :: [String] -> Int -> [Int]
getOxygenBinary [s] _ = [read [c] | c <- s]
getOxygenBinary ls i = getOxygenBinary (filter f ls) (i+1)
    where 
        g :: [Int]
        g = getGammaBinary ls (replicate (length $ head ls) 0)
    
        f :: String -> Bool
        f l = read [l !! i] == g !! i

getCo2Binary :: [String] -> Int -> [Int]
getCo2Binary [s] _ = [read [c] | c <- s]
getCo2Binary ls i = getCo2Binary (filter f ls) (i+1)
    where 
        e :: [Int]
        e = getEpsilonBinary $ getGammaBinary ls (replicate (length $ head ls) 0)
    
        f :: String -> Bool
        f l = read [l !! i] == e !! i

getEpsilonBinary :: [Int] -> [Int]
getEpsilonBinary gammaBinary = [if b == 0 then 1 else 0 | b <- gammaBinary]

getGammaBinary :: [String] -> [Int] -> [Int]
getGammaBinary [] counts = [if c < 0 then 0 else 1 | c <- counts]
getGammaBinary (l:ls) counts = getGammaBinary ls newCounts
    where
        changes :: [Int]
        changes = getChanges l
        newCounts :: [Int]
        newCounts = applyChanges counts changes

getChanges :: [Char] -> [Int]
getChanges cs = foldl (\ i c -> i ++ [if c == '0' then -1 else 1]) [] cs

applyChanges :: [Int] -> [Int] -> [Int]
applyChanges counts changes = [i + c | i <- counts | c <- changes]

getLines :: String -> IO [String]
getLines path = lines <$> readFile path

binaryToInt :: [Int] -> Int
binaryToInt bs = sum [b * 2^i | (b, i) <- zip bs indices]
    where 
        indices :: [Int]
        indices = reverse [0..length bs - 1]
