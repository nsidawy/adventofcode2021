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
type ARegister = M.Map Var Value

defaultRegister = (0,0,0,0)
defaultARegister = M.fromList [(X,I 0), (Y,I 0), (Z,I 0), (W,I 0)]

main = do
    instructions <- getInput "input.txt"
    start <- getArgs
    let maxMonad = (read (head start)) * 1000000000000 + 999936421899
    --print instructions
    let ci = chunkByInput instructions
    --print <$> runChunk ci defaultRegister 9 []
    print <$> findMaxMonad maxMonad instructions

chunkByInput :: [Instruction] -> [[Instruction]]
chunkByInput [] = []
chunkByInput is = (head is : takeWhile isNotInput (tail is)) : chunkByInput (dropWhile (\i -> isNotInput i) (tail is))
    where
        isNotInput (Inp _) = False
        isNotInput _ = True

findMaxMonad :: Int -> [Instruction] -> IO Int
findMaxMonad monad is
    | 0 `elem` monadInts = findMaxMonad (monad - 1) is
    | result == 0 = return monad
    | otherwise = do
        when (drop 9 monadInts == [9,9,9,9,9]) $ print $ show monad ++ "\t" ++ show result
        findMaxMonad (monad - 1) is
    where
        register = M.fromList [(X,0), (Y,0), (Z,0), (W,0)]
        monadInts = map C.digitToInt $ show monad
        (_,_,_,result) = run is defaultRegister monadInts

runChunk :: [[Instruction]] -> Register -> Int -> [Int] -> IO (Maybe [Int])
runChunk [] (_,_,_,z) _ e
    | z == 0 = return $ Just e
    | otherwise = do
        when (take 5 e == [9,9,9,9,9]) $ print (e,z) 
        return Nothing 
runChunk (i:is) r n e
    | n == 0 = return Nothing
    | otherwise = do
        answer <- L.find isJust <$> mapM (\x -> runChunk is r' x (n:e)) tries
        return $ fromMaybe Nothing answer
    where 
        r' = run i r [n]
        tries = [9,8,7,6,5,4,3,2,1]
        

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
