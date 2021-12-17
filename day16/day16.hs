import           Control.Monad
import qualified Data.List       as L
import           Data.List.Split

main = do
    binary <- getInput "ex5.txt"
    packets <- processPacket binary
    let vSum = sum [v | (v, _, _) <- fst packets]
    print binary
    print packets
    print vSum

getInput :: String -> IO [Int]
getInput path = do
    lines <- lines <$> readFile path
    return $ hexToBin $ head lines

processPacket :: [Int] -> IO ([(Int, Int, [Int])],[Int])
processPacket [] = return ([],[9,9,9]) 
processPacket bs 
    | t == 4 = return ([(v, t, lv)], lr)
    | ot == 0 = do
        let t0r = drop 22 bs 
        let t0l = binToDec $ take 15 $ drop 7 bs
        (packets0, t0rest) <- processType0 t0r t0l
        return ((v, t, take 16 $ drop 6 bs) : packets0, t0rest)
    | ot == 1 = do
        let t1r = drop 18 bs 
        let t1l = binToDec $ take 11 $ drop 7 bs
        (packets1, t1rest) <- processType1 t1r t1l
        return ((v, t, take 12 $ drop 6 bs) : packets1, t1rest)
    | otherwise = error "oops"
    where 
        v = binToDec $ take 3 bs
        t = binToDec $ take 3 $ drop 3 bs
        (lv, lr) = processLiteral $ drop 6 bs
        ot = bs !! 6

processType0 :: [Int] -> Int -> IO ([(Int, Int, [Int])],[Int])
processType0 bs 0 = return ([],bs)
processType0 bs l = do
    (packets, rest) <- processPacket bs
    let l' = length bs - length rest
    (npackets, nrest) <- processType0 rest (l - l') 
    return (packets ++ npackets, nrest)

processType1 :: [Int] -> Int -> IO ([(Int, Int, [Int])],[Int])
processType1 bs 0 = return ([],bs)
processType1 bs n = do
    (packets, rest) <- processPacket bs
    (npackets, nrest) <- processType1 rest (n-1) 
    return (packets ++ npackets, nrest)

processLiteral :: [Int] -> ([Int], [Int])
processLiteral bs 
    | hasNext = (value ++ nv, nrest)
    | otherwise = (value, rest)
    where
        hasNext = head bs == 1
        value = take 5 bs
        rest = drop 5 bs
        (nv, nrest) = processLiteral rest

binToDec :: [Int] -> Int
binToDec bs = sum [i * (2 ^ e) | (i, e) <- zip (reverse bs) [0..]]

hexToBin :: [Char] -> [Int]
hexToBin [] = []
hexToBin (c:cs) = b ++ hexToBin cs
    where 
        b 
            | c == '0' = [0,0,0,0]
            | c == '1' = [0,0,0,1]
            | c == '2' = [0,0,1,0]
            | c == '3' = [0,0,1,1]
            | c == '4' = [0,1,0,0]
            | c == '5' = [0,1,0,1]
            | c == '6' = [0,1,1,0]
            | c == '7' = [0,1,1,1]
            | c == '8' = [1,0,0,0]
            | c == '9' = [1,0,0,1]
            | c == 'A' = [1,0,1,0]
            | c == 'B' = [1,0,1,1]
            | c == 'C' = [1,1,0,0]
            | c == 'D' = [1,1,0,1]
            | c == 'E' = [1,1,1,0]
            | c == 'F' = [1,1,1,1]
