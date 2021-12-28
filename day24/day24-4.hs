import           Control.Monad
import           Control.Monad.Loops
import qualified Data.Char           as C
import qualified Data.List           as L
import qualified Data.Map            as M
import qualified Data.Vector            as V
import           Data.Maybe
import qualified Text.Read           as R
import System.Environment

consts = [         
    --div,xadd,yadd
    (1,11,6),--0 x13     
    (1,11,12),--1 x12  
    (1,15,8),--2 x3     
    (26,-11,7),--3 y2  
    (1,15,7),--4 x11     
    (1,15,12),--5 x10
    (1,14,2),--6 x7     
    (26,-7,15),--7 y6  
    (1,12,4),--8 x9     
    (26,-6,5),--9 x8   
    (26,-10,12),--10 y5  
    (26,-15,11),--11 y4   
    (26,-9,13),--12 y1   
    (26,0,7)]--13 y0

a = reverse [1..9]
vals1 = [[w0_13,w1_12,w2_3,w2_3-3,9,w5_10,w6_7,w6_7-5,w8_9,w8_9-2,w5_10+2,1,w1_12+3,w0_13+6] | 
    w0_13 <- reverse [1..3],
    w1_12 <- reverse [1..6],
    w2_3 <- reverse [4..9],
    w5_10 <- reverse [1..7],
    w6_7 <- reverse [6..9],
    w8_9 <- reverse [3..9]
    ]
vals2 = [[w0_13,w1_12,w2_3,w2_3-3,9,w5_10,w6_7,w6_7-5,w8_9,w8_9-2,w5_10+2,1,w1_12+3,w0_13+6] | 
    w0_13 <- [1..3],
    w1_12 <- [1..6],
    w2_3 <- [4..9],
    w5_10 <- [1..7],
    w6_7 <- [6..9],
    w8_9 <- [3..9]
    ]

main = do
    print $ head $ filter (\(x1,_)-> x1 == 0) $ map (\x -> runChunk consts x 0 x) vals1
    print $ head $ filter (\(x1,_)-> x1 == 0) $ map (\x -> runChunk consts x 0 x) vals2

runChunk :: [(Int,Int,Int)] -> [Int] -> Int -> [Int] -> (Int, [Int])
runChunk [] [] z all  = (z,all)
runChunk ((d,xa,ya):cs) (v:vs) z all = runChunk cs vs z' all
    where 
        x = (z `mod` 26) + xa
        z' = if x == v then z `div` d else (z `div` d) * 26 + (v + ya)
