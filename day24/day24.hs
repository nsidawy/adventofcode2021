import           Control.Monad
import           Control.Monad.Loops
import qualified Data.Char           as C
import qualified Data.List           as L
import qualified Data.Map            as M
import qualified Data.Vector            as V
import           Data.Maybe
import qualified Text.Read           as R

data Var =  W | X | Y | Z
    deriving (Show, Ord, Eq)
data Value = V Var | I Int | In Instruction | Input Int
    deriving (Show)
data Instruction =
    Inp Value
    | Add Value Value
    | Mul Value Value
    | Div Value Value
    | Mod Value Value
    | Eql Value Value
    deriving (Show)

type Register = M.Map Var Int
type ARegister = M.Map Var Value

defaultRegister = M.fromList [(X,0), (Y,0), (Z,0), (W,0)]
defaultARegister = M.fromList [(X,I 0), (Y,I 0), (Z,I 0), (W,I 0)]

main = do
    instructions <- getInput "input.txt"
    let maxMonad = 99999936421899
    --let maxMonad = 51111114521335
    --print instructions
    let collapsed = collapse instructions defaultARegister 0
    let evaluated = [(a, evaluate v $ V.fromList [7]) | (a,v) <- M.toList collapsed]
    --print $ collapsed M.! Z
    --print evaluated
    print <$> findMaxMonad maxMonad (collapsed M.! Z)

findMaxMonad :: Int -> Value -> IO Int
findMaxMonad monad v
    | 0 `elem` monadInts = findMaxMonad (monad - 1) v
    | result == 0 = return monad
    | otherwise = do
        print $ show monad ++ "\t" ++ show result
        findMaxMonad (monad - 1) v
    where
        register = M.fromList [(X,0), (Y,0), (Z,0), (W,0)]
        monadInts = V.fromList $ map C.digitToInt $ show monad
        result = evaluate v monadInts

collapse :: [Instruction] -> ARegister -> Int -> ARegister
collapse [] r n = r
collapse (i:is) r n = case i of
    Inp var -> let
        v' = Input n
        var' = case var of
            V a -> a
            _   -> error "bad input"
        r' = M.insert var' v' r
        in collapse is r' (n+1)
    Add (V var) val -> let
        aValue = r M.! var
        bValue = case val of
            V a -> r M.! a
            _   -> val
        v' = case (aValue, bValue) of
            (I 0, _)   -> bValue
            (_, I 0)   -> aValue
            (I a, I b) -> I (a+b)
            _          -> In (Add aValue bValue)
        r' = M.insert var v' r
        in collapse is r' n
    Mul (V var) val -> let
        aValue = r M.! var
        bValue = case val of
            V a -> r M.! a
            _   -> val
        v' = case (aValue, bValue) of
            (I 0, _)   -> I 0
            (_, I 0)   -> I 0
            (I 1, _)   -> bValue
            (_, I 1)   -> aValue
            (I a, I b) -> I (a*b)
            _          -> In (Mul aValue bValue)
        r' = M.insert var v' r
        in collapse is r' n
    Div (V var) val -> let
        aValue = r M.! var
        bValue = case val of
            V a -> r M.! a
            _   -> val
        v' = case (aValue, bValue) of
            (_, I 1)   -> aValue
            (I a, I b) -> I (a `div` b)
            _          -> In (Div aValue bValue)
        r' = M.insert var v' r
        in collapse is r' n
    Mod (V var) val -> let
        aValue = r M.! var
        bValue = case val of
            V a -> r M.! a
            _   -> val
        v' = case (aValue, bValue) of
            (_, I 1)   -> I 0
            (I a, I b) -> I (a `mod` b)
            _          -> In (Mod aValue bValue)
        r' = M.insert var v' r
        in collapse is r' n
    Eql (V var) val -> let
        aValue = r M.! var
        bValue = case val of
            V a -> r M.! a
            _   -> val
        v' = case (aValue, bValue) of
            (I a, I b)     -> I (if a == b then 1 else 0)
            (Input a, I b) -> if b > 9 then I 0 else In (Eql (Input a) (I b))
            (I a, Input b) -> if a > 9 then I 0 else In (Eql (I a) (Input b))
            _              -> In (Eql aValue bValue)
        r' = M.insert var v' r
        in collapse is r' n

evaluate :: Value -> V.Vector Int -> Int
evaluate (I i) is            = i
evaluate (Input i) is        = is V.! i
evaluate (In (Add v1 v2)) is = evaluate v1 is + evaluate v2 is
evaluate (In (Mul v1 v2)) is = evaluate v1 is * evaluate v2 is
evaluate (In (Div v1 v2)) is = evaluate v1 is `div` evaluate v2 is
evaluate (In (Mod v1 v2)) is = evaluate v1 is `mod` evaluate v2 is
evaluate (In (Eql v1 v2)) is = if evaluate v1 is == evaluate v2 is then 1 else 0
evaluate x is                = error$ show x

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
            | s == "x" = V X
            | s == "y" = V Y
            | s == "z" = V Z
            | s == "w" = V W

        parseValue :: String -> Value
        parseValue s
            | isJust (R.readMaybe s :: Maybe Int) = I (read s :: Int)
            | otherwise = parseVar s

--run :: [Instruction] -> Register -> [Int] -> Register
--run [] r _ = r
--run (i:is) r inp = case i of
--    Inp var -> let
--        v' = head inp
--        r' = M.insert var v' r
--        in run is r' $ tail inp
--    Add var val -> let
--        v' = getVar var + getValue val
--        r' = M.insert var v' r
--        in run is r' inp
--    Mul var val -> let
--        v' = getVar var * getValue val
--        r' = M.insert var v' r
--        in run is r' inp
--    Div var val -> let
--        v' = getVar var `div` getValue val
--        r' = M.insert var v' r
--        in run is r' inp
--    Mod var val -> let
--        v' = getVar var `mod` getValue val
--        r' = M.insert var v' r
--        in run is r' inp
--    Eql var val -> let
--        v' = if getVar var == getValue val then 1 else 0
--        r' = M.insert var v' r
--        in run is r' inp
--    where
--        getValue v = case v of
--            I x -> x
--            V x -> getVar x
--        getVar v = r M.! v
