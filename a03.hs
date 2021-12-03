-- https://adventofcode.com/2021/day/3

module A03 where

import qualified Data.IntMap.Strict as M

main = do
    ss <- getContents <&> lines
    answer 1 $ part1 ss
    answer 2 (part2 False ss, part2 True ss)

answer :: Int -> (Int,Int) -> IO ()
answer i (a,b) = printf "Part %d: %d = %d * %d\n" i (a*b) a b

part1 ss = (g,e) where
    g = foldl' gamma 0 $ M.elems totals
    e = 2^(M.size totals) - g - 1
    (n,totals) = foldl' parse (0,M.empty) ss
    gamma g v = g * 2 + if v * 2 < n then 0 else 1
    parse (!n,!a) s = (n+1,b) where
        b = M.unionWith (+) a $ 
            M.fromAscList . zip [0..] $
            subtract (ord '0') . ord <$> s

part2 isCO2 ss = go ss 0 where
    go [s] i = foldl' f i s where f i c = i*2 + digitToInt c
    go ss i = if (zn <= qn) == isCO2
        then go zz $ i*2
        else go qq $ i*2 + 1
      where
        ((zn,zz),(qn,qq)) = split ss

split = go 0 [] 0 [] where
    go zn zz qn qq = \case
        [] -> ((zn,zz),(qn,qq))
        x : xx -> if head x == '0'
            then go (zn+1) (tail x:zz) qn qq xx
            else go zn zz (qn+1) (tail x:qq) xx
