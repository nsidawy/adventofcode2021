import           Control.Monad
import qualified Data.List       as L

main = do
    binary <- getInput "input.txt"
    --let binary = hexToBin "9C0141080250320F1802104A08"
    let (packets, _) = readPacket binary
    print packets
    let vSum = sum [v | (v, _, _) <- packets]
    print vSum
    print $ calculatePacket packets

calculatePacket :: [(Int, Int, Int)] -> (Int, [(Int, Int, Int)])
calculatePacket [] = (0,[])
calculatePacket ((v,t,i):ps)
    | t == 0 = (sum values, rest)
    | t == 1 = (product values, rest)
    | t == 2 = (minimum values, rest)
    | t == 3 = (maximum values, rest)
    | t == 4 = (i, ps)
    | t == 5 = (if head bvalues > bvalues !! 1 then 1 else 0, brest)
    | t == 6 = (if head bvalues < bvalues !! 1 then 1 else 0, brest)
    | t == 7 = (if head bvalues == bvalues !! 1 then 1 else 0, brest)
    where
        (values, rest) = calculatePacket' ps i
        (bvalues, brest) = calculatePacket' ps 2

calculatePacket' :: [(Int, Int, Int)] -> Int -> ([Int], [(Int, Int, Int)])
calculatePacket' ps 0 = ([], ps)
calculatePacket' ps i = (value : values, nrest)
    where 
        (value, rest) = calculatePacket ps
        (values, nrest) = calculatePacket' rest (i-1)

readPacket :: [Int] -> ([(Int, Int, Int)],[Int])
readPacket bs 
    | t == 4 = ([(v, t, lv)], lr)
    | ot == 0 = ((v, t, t0c) : packets0, t0rest)
    | ot == 1 = ((v, t, t1l) : packets1, t1rest)
    where 
        v = binToDec $ take 3 bs
        t = binToDec $ take 3 $ drop 3 bs
        (lv, lr) = readLiteral $ drop 6 bs

        ot = bs !! 6

        t0r = drop 22 bs 
        t0l = binToDec $ take 15 $ drop 7 bs
        (packets0, t0c, t0rest) = readType0 t0r t0l 0

        t1r = drop 18 bs 
        t1l = binToDec $ take 11 $ drop 7 bs
        (packets1, t1rest) = readType1 t1r t1l

readType0 :: [Int] -> Int -> Int -> ([(Int, Int, Int)],Int,[Int])
readType0 bs 0 c = ([],c,bs)
readType0 bs l c = (packets ++ npackets, count, nrest)
    where 
        (packets, rest) = readPacket bs
        l' = length bs - length rest
        (npackets, count, nrest) = readType0 rest (l - l') (c+1)

readType1 :: [Int] -> Int -> ([(Int, Int, Int)],[Int])
readType1 bs 0 = ([],bs)
readType1 bs n = (packets ++ npackets, nrest)
    where 
        (packets, rest) = readPacket bs
        (npackets, nrest) = readType1 rest (n-1) 

readLiteral :: [Int] -> (Int, [Int])
readLiteral bs = (binToDec b, rest)
    where
        (b, rest) = readLiteral' bs

readLiteral' :: [Int] -> ([Int], [Int])
readLiteral' bs 
    | hasNext = (value ++ nv, nrest)
    | otherwise = (value, rest)
    where
        hasNext = head bs == 1
        value = tail $ take 5 bs
        rest = drop 5 bs
        (nv, nrest) = readLiteral' rest

binToDec :: [Int] -> Int
binToDec bs = sum [i * (2 ^ e) | (i, e) <- zip (reverse bs) [0..]]

getInput :: String -> IO [Int]
getInput path = do
    lines <- lines <$> readFile path
    return $ hexToBin $ head lines

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
