module H21 where

import Linear.V2
import Control.Monad.Trans ( lift )
import Control.Monad.State ( evalStateT, get, gets, put )
import qualified Data.Map.Strict as M
import qualified Data.Array as A

main = do
    [_,p1] <- parse
    [_,p2] <- parse
    solve p1 p2
    print $ part2 p1 p2
  where
    parse :: IO [Int]
    parse = map read . words . map tr <$> getLine
    tr c = if isDigit c then c else ' '

data Player = P1 | P2 deriving ( Eq, Ord, Ix, Show )
opp P1 = P2
opp P2 = P1

threes = 
    let tri = [1..3] in
    M.assocs . M.fromListWith (+) $ 
    map (,1) [ a+b+c | a <- tri, b <- tri, c <- tri ]

win1 = V2 1 0
win2 = V2 0 1

based b n = mod (n-1) b + 1

part2 a b =
    ary A.! (P1,a,b,0,0)
  where
    bds = ((P1,1,1,0,0),(P2,10,10,20,20))
    ary = A.listArray bds $ go <$> range bds
    go (p,a,b,s,t) = sum $ f <$> threes where
        f (v,n) = V2 n n *
            let a' = based 10 $ a + v in
            let s' = s + a' in
            if s' < 21 then 
                ary A.! (opp p, b, a', t, s')
            else 
                if p == P1 then win1 else win2

solve a b = do
    ans <- flip evalStateT (0,0) $ go 0 0 a b
    print ans
  where
    roll = get >>= \(z,n) -> put (mod (z+1) 10, n+1) >> pure (z+1)
    go s1 s2 p1 p2
        | s1 >= 1000 = gets snd >>= \n -> pure (s2 * n)
        | s2 >= 1000 = gets snd >>= \n -> pure (s1 * n)
        | otherwise = do
            a <- roll
            b <- roll
            c <- roll
            let p = mod (p1 + a + b + c - 1) 10 + 1
            let s = s1 + p
            go s2 s p2 p
