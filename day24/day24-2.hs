import           Control.Monad
import           Control.Monad.Loops
import qualified Data.Char           as C
import qualified Data.List           as L
import qualified Data.Map            as M
import           Data.Maybe
import qualified Data.Vector         as V
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

type Register = (Value,Value,Value,Value)
type Input = M.Map Int Int

defaultRegister = (I 0,I 0,I 0,I 0)

main = do
    instructions <- getInput "input.txt"
    --let evaluated = [(r, evaluate (getRegister collapsed r) $ V.fromList [7]) | r <- [W,X,Y,Z]]
    --print evaluated
    let chunks = chunkByInput instructions 
    let collapsed2 = map (\(n,i) -> collapse i defaultRegister n) (zip [0..] chunks)
    mapM (\c -> do 
        print c
        print "\n") collapsed2
    --print <$> findMaxMonad maxMonad (getRegister collapsed Z)

chunkByInput :: [Instruction] -> [[Instruction]]
chunkByInput [] = []
chunkByInput is = (head is : takeWhile isNotInput (tail is)) : chunkByInput (dropWhile (\i -> isNotInput i) (tail is))
    where
        isNotInput (Inp _) = False
        isNotInput _       = True

getInputs :: Value -> [Int]
getInputs (I i)            = []
getInputs (Input i)        = [i]
getInputs (In (Add v1 v2)) = L.nub $ getInputs v1 ++ getInputs v2
getInputs (In (Mul v1 v2)) = L.nub $ getInputs v1 ++ getInputs v2
getInputs (In (Div v1 v2)) = L.nub $ getInputs v1 ++ getInputs v2
getInputs (In (Mod v1 v2)) = L.nub $ getInputs v1 ++ getInputs v2
getInputs (In (Eql v1 v2)) = L.nub $ getInputs v1 ++ getInputs v2
getInputs x                = error $ show x

findMaxMonad :: Int -> Value -> IO Int
findMaxMonad monad v
    | 0 `elem` monadInts = findMaxMonad (monad - 1) v
    | result == 0 = return monad
    | otherwise = do
        print $ show monad ++ "\t" ++ show result
        findMaxMonad (monad - 1) v
    where
        monadInts = M.fromList $ zip [0..] (map C.digitToInt $ show monad)
        result = evaluate v monadInts

collapse :: [Instruction] -> Register -> Int -> Register
collapse [] r n = r
collapse (i:is) r n = case i of
    Inp var -> let
        newValue = Input n
        var' = case var of
            V a -> a
            _   -> error "bad input"
        r' = setRegister r var' newValue
        in collapse is r' (n+1)
    Add (V a) b -> let
        aValue = getRegister r a
        bValue = case b of
            V a -> getRegister r a
            _   -> b
        newValue = case (aValue, bValue) of
            (I 0, _)   -> bValue
            (_, I 0)   -> aValue
            (I a, I b) -> I (a+b)
            _          -> In (Add aValue bValue)
        r' = setRegister r a newValue
        in collapse is r' n
    Mul (V a) b -> let
        aValue = getRegister r a
        bValue = case b of
            V a -> getRegister r a
            _   -> b
        newValue = case (aValue, bValue) of
            (I 0, _)   -> I 0
            (_, I 0)   -> I 0
            (I 1, _)   -> bValue
            (_, I 1)   -> aValue
            (I a, I b) -> I (a*b)
            _          -> In (Mul aValue bValue)
        r' = setRegister r a newValue
        in collapse is r' n
    Div (V a) b -> let
        aValue = getRegister r a
        bValue = case b of
            V a -> getRegister r a
            _   -> b
        newValue = case (aValue, bValue) of
            (_, I 1)   -> aValue
            (I 0, _)   -> I 0
            (I a, I b) -> I (a `div` b)
            _          -> In (Div aValue bValue)
        r' = setRegister r a newValue
        in collapse is r' n
    Mod (V a) b -> let
        aValue = getRegister r a
        bValue = case b of
            V a -> getRegister r a
            _   -> b
        newValue = case (aValue, bValue) of
            (I 0, _)   -> I 0
            (_, I 1)   -> I 0
            (I a, I b) -> I (a `mod` b)
            _          -> In (Mod aValue bValue)
        r' = setRegister r a newValue
        in collapse is r' n
    Eql (V a) b -> let
        aValue = getRegister r a
        bValue = case b of
            V a -> getRegister r a
            _   -> b
        newValue = case (aValue, bValue) of
            (I a, I b)     -> I (if a == b then 1 else 0)
            (Input a, I b) -> if b > 9 then I 0 else In (Eql (Input a) (I b))
            (I a, Input b) -> if a > 9 then I 0 else In (Eql (I a) (Input b))
            (_,_)      -> let
                ins = In $ Eql aValue bValue
                inputs = getInputs ins
                x = map M.fromList $ sequence [[(i,n) | n <- [1..9]] | i <- inputs]
                outs = L.nub $ map (evaluate ins) x
                in ins
        r' = setRegister r a newValue
        in collapse is r' n

evaluate :: Value -> Input -> Int
evaluate (I i) is            = i
evaluate (Input i) is        = is M.! i
evaluate (In (Add v1 v2)) is = evaluate v1 is + evaluate v2 is
evaluate (In (Mul v1 v2)) is = evaluate v1 is * evaluate v2 is
evaluate (In (Div v1 v2)) is = evaluate v1 is `div` evaluate v2 is
evaluate (In (Mod v1 v2)) is = evaluate v1 is `mod` evaluate v2 is
evaluate (In (Eql v1 v2)) is = if evaluate v1 is == evaluate v2 is then 1 else 0
evaluate x is                = error$ show x

getRegister :: Register -> Var -> Value
getRegister (w,x,y,z) W = w
getRegister (w,x,y,z) X = x
getRegister (w,x,y,z) Y = y
getRegister (w,x,y,z) Z = z

setRegister :: Register -> Var -> Value -> Register
setRegister (w,x,y,z) W v = (v,x,y,z)
setRegister (w,x,y,z) X v = (w,v,y,z)
setRegister (w,x,y,z) Y v = (w,x,v,z)
setRegister (w,x,y,z) Z v = (w,x,y,v)

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
