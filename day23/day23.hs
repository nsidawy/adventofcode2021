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
type Memo = M.Map ([Position], Int) Int

--final = [(A,2,1), (A,2,2), (B,4,1), (B,4,2), (C,6,1), (C,6,2), (D,8,1), (D,8,2)]
final = [(A,2,1), (A,2,2), (A,2,3), (A,2,4),
        (B,4,1), (B,4,2), (B,4,3), (B,4,4),
        (C,6,1), (C,6,2), (C,6,3), (C,6,4),
        (D,8,1), (D,8,2), (D,8,3), (D,8,4)]
--maxY = 2
maxY = 4
hallways = [0,1,3,5,7,9,10]

main = do
    --let positions = [(A,2,2), (B,2,1), (D,4,2), (C,4,1), (C,6,2), (B,6,1), (A,8,2), (D,8,1)]
    --let positions = [(B,2,2), (C,2,1), (A,4,2), (D,4,1), (D,6,2), (A,6,1), (C,8,2), (B,8,1)]
    --let positions = [(A,2,4), (D,2,3), (D,2,2), (B,2,1),
    --                (D,4,4), (B,4,3), (C,4,2), (C,4,1),
    --                (C,6,4), (A,6,3), (B,6,2), (B,6,1),
    --                (A,8,4), (C,8,3), (A,8,2), (D,8,1)]
    let positions = [(B,2,4), (D,2,3), (D,2,2), (C,2,1),
                    (A,4,4), (B,4,3), (C,4,2), (D,4,1),
                    (D,6,4), (A,6,3), (B,6,2), (A,6,1),
                    (C,8,4), (C,8,3), (A,8,2), (B,8,1)]
    (minCost,_) <- solve (L.sort positions) 0 10000000000 M.empty
    print minCost

solve :: [Position] -> Int -> Int -> Memo -> IO (Int, Memo)
solve ps cost minCost memo
    | isFinal ps = return (cost, memo)
    | null allMoves = return (1000000000000, memo)
    | cost > minCost = return (1000000000000, memo)
    | (ps, cost) `M.member` memo = return (memo M.! (ps, cost), memo)
    | otherwise = do
        --print $ "Cost:" ++ show cost ++ "," ++ show minCost
        --print $ "Positions:" ++ show ps
        --print $ "Moves:" ++ show (filter (\(p,ms) -> not $ null ms) allMovesGroup)
        (cost', memo') <- foldM agg (minCost, memo) allMoves
        let memo'' = M.insert (ps,cost) cost' memo'
        return (cost',memo'')
    where 
        allMovesGroup = map (\p -> (p, getMoves ps p)) ps
        allMoves = concat [[(p,m) | m <- ms] | (p,ms) <- allMovesGroup]
        agg (sc,sm) (p,m) = do
            (cc', mm') <- solve (applyMove ps p m) (cost + getCost p m) sc sm
            return (minimum [sc, cc'], mm')
    
getMoves :: [Position] -> Position -> [Position]
getMoves ps (b,x,y) 
    --if a bug is blocked in its room, then it can't move
    | y > 1 && any (\(_,x',y') -> x' == x && y' < y ) ps' = []
    --If a bug is in its home room and no other bug type is there, then it is done
    | b == A && x == 2 && not (any (\(b',x',_) -> b' /= A && x' == 2) ps') = []
    | b == B && x == 4 && not (any (\(b',x',_) -> b' /= B && x' == 4) ps') = []
    | b == C && x == 6 && not (any (\(b',x',_) -> b' /= C && x' == 6) ps') = []
    | b == D && x == 8 && not (any (\(b',x',_) -> b' /= D && x' == 8) ps') = []
    --In hallway then can't move if the room is occupied by another bug type
    | y == 0 && not canRoom = []
    --In hallway and nothing between it and its room
    | y == 0 && not (any (\(_,x',y') -> y' == 0 && isBetween x' x targetX) ps') = [(b,targetX,targetY)] 
    | otherwise =
        [(b,targetX,targetY) | canRoom && not (any (\(_,x',y') -> y' == 0 && isBetween x' x targetX) ps)]
        ++ [(b,h,0)
            | h <- hallways
            , not (any (\(_,x',y') -> y' == 0 && isBetween x' x h) ps)
            , y /= 0] 
    where 
        ps' = filter (/=(b,x,y)) ps
        targetX 
            | b == A = 2
            | b == B = 4
            | b == C = 6
            | b == D = 8
        --bug target Y will be as low as possible
        targetY = minimum $ map (\(b',x',y') -> if b' == b && x' == targetX then y' - 1 else maxY) ps'
        --if another bug type is in its room, then it can't go there
        canRoom = not $ any (\(b',x',_) -> b' /= b && x' == targetX) ps'
        --helper
        isBetween x xStart xEnd = (xStart < x && x <= xEnd) || (xEnd <= x && x < xStart)

getCost :: Position -> Position -> Int
getCost (b,ox,oy) (_,nx,ny) = bcost * (abs (nx - ox) + ny + oy)
    where 
        bcost 
            | b == A = 1
            | b == B = 10
            | b == C = 100
            | b == D = 1000

isFinal :: [Position] -> Bool
isFinal ps = final == ps

applyMove :: [Position] -> Position -> Position -> [Position]
applyMove ps op np = L.sort $ np : [p | p <- ps, p /= op]

getInput :: String -> IO [String]
getInput path = do
    lines <- lines <$> readFile path
    return lines
