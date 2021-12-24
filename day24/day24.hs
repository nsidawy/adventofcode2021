import           Control.Monad
import           Control.Monad.Loops
import qualified Data.List       as L
import qualified Data.Map        as M
import qualified Data.Char     as C
import Data.Maybe
import qualified Text.Read as R

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

type Register = M.Map Var Int

defaultRegister = M.fromList [(X,0), (Y,0), (Z,0), (W,0)]

main = do
    instructions <- getInput "input.txt"
    let maxMonad = 100000000000000 - 1
    print instructions
    print $ findMaxMonad maxMonad instructions
    print "Done"

findMaxMonad :: Int -> [Instruction] -> Int
findMaxMonad monad ins 
    | 0 `elem` monadInts = findMaxMonad (monad - 1) ins 
    | result M.! Z == 0 = monad
    | otherwise = findMaxMonad (monad - 1) ins
    where 
        register = M.fromList [(X,0), (Y,0), (Z,0), (W,0)]
        monadInts = map C.digitToInt $ show monad
        result = run ins defaultRegister monadInts

run :: [Instruction] -> Register -> [Int] -> Register
run [] r _ = r
run (i:is) r inp = case i of
    Inp var -> let
        newVarValue = head inp
        r' = M.insert var newVarValue r
        in run is r' $ tail inp
    Add var val -> let
        newVarValue = getVar var + getValue val
        r' = M.insert var newVarValue r
        in run is r' inp
    Mul var val -> let
        newVarValue = getVar var * getValue val
        r' = M.insert var newVarValue r
        in run is r' inp
    Div var val -> let
        newVarValue = getVar var `div` getValue val
        r' = M.insert var newVarValue r
        in run is r' inp
    Mod var val -> let
        newVarValue = getVar var `mod` getValue val
        r' = M.insert var newVarValue r
        in run is r' inp
    Eql var val -> let
        newVarValue = if getVar var == getValue val then 1 else 0
        r' = M.insert var newVarValue r
        in run is r' inp
    where
        getValue v = case v of
            I x -> x
            V x -> getVar x
        getVar v = r M.! v

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
            | otherwise = V $ parseVar s
