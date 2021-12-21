import           Control.Monad
import qualified Data.List       as L
import qualified Data.Map        as M

type Memo = M.Map (Int,Int,Int,Int,Bool) (Int,Int)

main = do
    --let (p1, p2) = (4,8)
    let (p1, p2) = (6,4)
    let (s1, s2, dieCount) = play p1 p2 0 0 1 0 True
    let part1 = minimum [s1,s2] * dieCount
    print part1 
    let (w1,w2,_) = play2 p1 p2 0 0 True M.empty
    print (w1,w2)

play :: Int -> Int -> Int -> Int -> Int -> Int -> Bool -> (Int,Int,Int)
play p1 p2 s1 s2 curDie dieCount isPlay1
    | s1 >= 1000 || s2 >= 1000 = (s1,s2, dieCount)
    | otherwise = play p1' p2' s1' s2' (curDie+3) (dieCount+3) (not isPlay1)
        where
            moves = (curDie * 3) + 3
            p1' = if isPlay1 then (p1 + moves) `mod` 10 else p1
            p2' = if not isPlay1 then (p2 + moves) `mod` 10 else p2
            s1' =
                let s = if p1' == 0 then 10 else p1'
                in if isPlay1 then s1 + s else s1
            s2' = 
                let s = if p2' == 0 then 10 else p2'
                in if not isPlay1 then s2 + s else s2

play2 :: Int ->  Int -> Int -> Int -> Bool -> Memo -> (Int,Int, Memo)
play2 p1 p2 s1 s2 isPlay1 m
    | s1 >= 21 = (1,0,m)
    | s2 >= 21 = (0,1,m)
    | (p1,p2,s1,s2,isPlay1) `M.member` m = 
        let (s1',s2') = m M.! (p1,p2,s1,s2,isPlay1)
        in (s1',s2', m)
    | otherwise = (s1n, s2n, m')
        where
            p1' x = if isPlay1 then (p1 + x) `mod` 10 else p1
            p2' x = if not isPlay1 then (p2 + x) `mod` 10 else p2
            s1' x = 
                let s = if p1' x == 0 then 10 else p1' x
                in if isPlay1 then s1 + s else s1
            s2' x = 
                let s = if p2' x == 0 then 10 else p2' x
                in if not isPlay1 then s2 + s else s2
            play2' x me = play2 (p1' x) (p2' x) (s1' x) (s2' x) (not isPlay1) me
            agg (s1,s2,m) (x,mult) = 
                let (s1',s2', m') = play2' x m
                in (s1 + s1'*mult, s2 + s2'*mult, m')
            (s1n,s2n,mAgg) = foldl agg (0,0,m) (zip [3..9] [1,3,6,7,6,3,1])
            m' = M.insert (p1,p2,s1,s2,isPlay1) (s1n, s2n) mAgg
