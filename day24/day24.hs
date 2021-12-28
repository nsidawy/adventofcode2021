import           Control.Monad
import           Control.Monad.Loops
import qualified Data.Char           as C
import qualified Data.List           as L
import qualified Data.Map            as M
import qualified Data.Vector            as V
import           Data.Maybe
import qualified Text.Read           as R
import System.Environment

data Var =  W | X | Y | Z
    deriving (Show, Ord, Eq)
data Value = V Var | I Int
    deriving (Show)
data Instruction =
    Inp Var
    | Add Var Value
    | Mul Var Value
    | Div Var Value
    | Mod Var Value
    | Eql Var Value
    deriving (Show)

type Register = (Int,Int,Int,Int)

defaultRegister = (0,0,0,0)
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
    instructions <- getInput "input.txt"
    print $ getAnswer vals1 instructions
    print $ getAnswer vals2 instructions

getAnswer :: [[Int]] -> [Instruction] -> [Int]
getAnswer guesses instructions = fst $ head $ filter (\(a,(_,_,_,z)) -> z == 0) $ map (\m -> (m, run instructions defaultRegister m)) guesses

run :: [Instruction] -> Register -> [Int] -> Register
run [] r _ = r
run (i:is) r inp = case i of
    Inp var -> let
        v' = head inp
        r' = setRegister r var v'
        in run is r' $ tail inp
    Add var val -> let
        v' = getVar var + getValue val
        r' = setRegister r var v'
        in run is r' inp
    Mul var val -> let
        v' = getVar var * getValue val
        r' = setRegister r var v'
        in run is r' inp
    Div var val -> let
        v' = getVar var `div` getValue val
        r' = setRegister r var v'
        in run is r' inp
    Mod var val -> let
        v' = getVar var `mod` getValue val
        r' = setRegister r var v'
        in run is r' inp
    Eql var val -> let
        v' = if getVar var == getValue val then 1 else 0
        r' = setRegister r var v'
        in run is r' inp
    where
        getValue v = case v of
            I x -> x
            V x -> getVar x
        getVar v = getRegister r v

getRegister :: Register -> Var -> Int
getRegister (w,x,y,z) v 
    | v == W = w
    | v == X = x
    | v == Y = y
    | v == Z = z

setRegister :: Register -> Var -> Int -> Register
setRegister (w,x,y,z) v i
    | v == W = (i,x,y,z) 
    | v == X = (w,i,y,z) 
    | v == Y = (w,x,i,z) 
    | v == Z = (w,x,y,i) 

getInput :: String -> IO [Instruction]
getInput path = do
    lines <- lines <$> readFile path
    return $ map parseLine lines

parseLine :: String -> Instruction
parseLine s
    | ins == "inp" = Inp (parseVar $ head rest)
    | ins == "add" = Add (parseVar $ head rest) (parseValue $ last rest)
    | ins == "mul" = Mul (parseVar $ head rest) (parseValue $ last rest)
    | ins == "div" = Div (parseVar $ head rest) (parseValue $ last rest)
    | ins == "mod" = Mod (parseVar $ head rest) (parseValue $ last rest)
    | ins == "eql" = Eql (parseVar $ head rest) (parseValue $ last rest)
    where
        (ins:rest) = words s

        parseVar s
            | s == "x" = X
            | s == "y" = Y
            | s == "z" = Z
            | s == "w" = W

        parseValue :: String -> Value
        parseValue s
            | isJust (R.readMaybe s :: Maybe Int) = I (read s :: Int)
            | otherwise = V (parseVar s)
