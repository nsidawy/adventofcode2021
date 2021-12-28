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
    (1,11,6),      
    (1,11,12),     
    (1,15,8),      
    (26,-11,7),    
    (1,15,7),      
    (1,15,12),     
    (1,14,2),      
    (26,-7,15),    
    (1,12,4),      
    (26,-6,5),     
    (26,-10,12),   
    (26,-15,11),   
    (26,-9,13),    
    (26,0,7)]

main = do
    print <$> runChunk consts 0 1 []

runChunk :: [(Int,Int,Int)] -> Int -> Int -> [Int] -> IO (Maybe [Int])
runChunk [] z _ e
    | z == 0 = return $ Just e
    | otherwise = do
        when (take 6 e == [9,9,9,9,9,9]) $ print (e,z) 
        return Nothing 
runChunk ((d,xa,ya):cs) z n e
    | n > 9 = return Nothing
    | otherwise = do
        answer <- runChunk cs z' 1 (n:e)
        if isJust answer then return answer else runChunk ((d,xa,ya):cs) z (n+1) e
    where 
        x = (z `mod` 26) + xa
        y = if x == n then 1 else 26
        z' = (z `div` d) * y + (n + ya)
