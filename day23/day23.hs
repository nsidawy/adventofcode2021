import           Control.Monad
import           Control.Monad.Loops
import qualified Data.List       as L
import qualified Data.Map        as M
import qualified Data.Vector     as V
import Data.List.Split
import           Text.Printf

data Bug = A | B | C | D
    deriving (Show, Eq, Ord)
type Position = (Bug,Int,Int) 
type Memo = M.Map [Position] Int

final = [(A,2,2), (A,2,1), (B,4,2), (B,4,1), (C,6,2), (C,6,1), (D,8,2), (D,8,1)]
hallways = [0,1,3,5,7,8,9]

main = do
    let positions = [(A,2,2), (B,2,1), (D,4,2), (C,4,1), (C,6,2), (B,6,1), (A,8,2), (D,8,1)]
    val <- solve positions 0 M.empty
    print $ applyMove positions (A,2,2) (A,3,4)
    print $ getCost (C,0,1) (C,2,2)
    print val
    print "done"

sortPositions :: [Position] -> [Position]
sortPositions = L.sortBy (\(_,x1,y1) (_,x2,y2) -> if x1 < x2 || (x1 == x2 && y1 < y2) then LT else GT)

solve :: [Position] -> Int -> Memo -> IO (Int, Memo)
solve ps cost memo
    | isFinal ps = return (cost, memo)
    | all (\(_,ms) -> null ms) allMoves = return (1000000000000, memo)
    | otherwise = do
        print ps
        print allMoves
        solves <- mapM (\(p,ms) -> mapM (\m -> solve (applyMove ps p m) (cost + getCost p m) memo) ms) allMoves
        let (cost',_) = L.minimumBy (\(c1,_) (c2,_) -> if c1 <= c2 then LT else GT) $ concat solves
        let memo' = M.insert (L.sort ps) cost' memo
        return (cost',memo')
    where 
        allMoves = map (\p -> (p, getMoves ps p)) ps
    
getMoves :: [Position] -> Position -> [Position]
getMoves ps (b,x,y) 
    --if a bug is blocked in its room, then it can't move
    | y == 2 && any (\(_,x',y') -> (x',y') == (x,1)) ps' = []
    --If a bug is in its home room and no other bug type is there, then it is done
    | b == A && x == 2 && not (any (\(b',x',_) -> b' /= A && x' == 2) ps') = []
    | b == B && x == 4 && not (any (\(b',x',_) -> b' /= B && x' == 4) ps') = []
    | b == C && x == 6 && not (any (\(b',x',_) -> b' /= C && x' == 6) ps') = []
    | b == D && x == 8 && not (any (\(b',x',_) -> b' /= D && x' == 8) ps') = []
    --In hallway then can't move if the room is occupied by another bug type
    | y == 0 && not canRoom = []
    --In hallway and nothing between it and room in hallway
    | y == 0 && not (any (\(_,x',y') -> y' == 0 && isBetween x' x targetX) ps') = [(b,targetX,targetY)] 
    | otherwise = [(b,h,0)
            | h <- hallways
            , not (any (\(_,x',y') -> y' == 0 && isBetween x' x h) ps)
            , y /= 0] 
         ++ if any (\(_,x',y') -> y' == 0 && isBetween x' x targetX) ps then [] else [(b,targetX,targetY)]
    where 
        ps' = filter (/=(b,x,y)) ps
        targetX 
            | b == A = 2
            | b == B = 4
            | b == C = 6
            | b == D = 8
        --bug target Y will be as low as possible
        targetY = if any (\(b',x',_) -> b' == b && x' == targetX) ps' then 1 else 2
        --if another bug type is in its room, then it can't go there
        canRoom = not $ any (\(b',x',_) -> b' /= b && x' == targetX) ps'

        isBetween x x1 x2 = (x1 < x && x < x2) || (x2 < x && x < x1)

getCost :: Position -> Position -> Int
getCost (b,ox,oy) (_,nx,ny) = bcost * (abs (nx - ox) + abs (ny - oy))
    where 
        bcost 
            | b == A = 1
            | b == B = 10
            | b == C = 100
            | b == D = 1000

isFinal :: [Position] -> Bool
isFinal ps = length (final `L.intersect` ps) == 8

applyMove :: [Position] -> Position -> Position -> [Position]
applyMove ps op np = np : [p | p <- ps, p /= op]

getInput :: String -> IO [String]
getInput path = do
    lines <- lines <$> readFile path
    return lines
