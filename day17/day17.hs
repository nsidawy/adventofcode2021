import           Control.Monad
import qualified Data.List       as L

main = do
    --let (minX,maxX,minY,maxY) = (20,30,-10,-5)
    let (minX,maxX,minY,maxY) = (34,67,-215,-186)
    let yVelocityMax = minY * (-1)
    let takeSize = yVelocityMax * 2 + 1

    let xs = [0..maxX]
    let yPoss = [minY..yVelocityMax] 

    let countsByX = [
            (x, validXCounts) | x <- xs,
            let validXCounts = take takeSize $ isXValid x 0 minX maxX 0,
            not $ null validXCounts]
    let allXCounts = concat [[(x,c) | c <- cs] | (x, cs) <- countsByX]
    let validVelocities = L.sort . L.nub $ [
            (x,y,c) | (x,c) <- allXCounts,
            y <- yPoss,
            isYValid y minY maxY c]
    let maxHeight = maximum $ [(y * (y + 1)) `div` 2 | (_,y,c) <- validVelocities]
    print maxHeight

    let uniqueSolutions = L.nub $ [(x,y) | (x,y,_) <- validVelocities]
    print $ length uniqueSolutions

isXValid :: Int -> Int -> Int -> Int -> Int -> [Int]
isXValid x curX minX maxX cnt 
    | curX > maxX = []
    | curX < minX && x == 0 = []
    | curX < minX = rest
    | curX >= minX && curX <= maxX && x == 0 = [cnt..]
    | curX >= minX && curX <= maxX = cnt : rest
    where 
        x' = if x /= 0 then x - 1 else x
        rest = isXValid x' (curX + x) minX maxX (cnt+1)

isYValid :: Int -> Int -> Int -> Int -> Bool
isYValid y minY maxY cnt = final >= minY && final <= maxY
    where 
        final = getFinalY y 0 cnt

getFinalY :: Int -> Int -> Int -> Int
getFinalY y cur 0 = cur 
getFinalY y cur c = getFinalY (y-1) (cur + y) (c-1)
