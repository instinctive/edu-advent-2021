module H09 where

import Linear.V2
import qualified Data.Array as A
import qualified Data.Set as S

main = do
    ss <- getContents <&> lines
    let (one,two) = solve ss
    printf "Part 1: %d\n" one
    printf "Part 2: %d\n" two

solve :: [String] -> (Int,Int)
solve ss =
    (one,two)
  where
    nrows = length ss
    ncols = length (head ss)
    zrow = replicate (ncols+2) 9
    zcol = 9
    bracket x xx = [x] <> xx <> [x]
    ary = A.listArray (V2 0 0, V2 (nrows+1) (ncols+1)) . concat $
        bracket zrow
        [ bracket zcol $ digitToInt <$> s | s <- ss ]
    depth = (ary A.!)
    range = A.range (V2 1 1, V2 nrows ncols)
    lows = filter isLow range
    adj = let n = -1 in [V2 1 0,V2 0 1,V2 n 0,V2 0 n]
    isLow v = all f adj where
        f d = depth (v + d) > x
        x = depth v
    one = length lows + sum (depth <$> lows)
    singles = S.fromList $ filter ((<9).depth) range
    two = go [] singles
    go bb s = case S.minView s of
        Nothing -> product . take 3 . sortBy (comparing Down) $ bb
        Just (v,s') -> dfs 1 s' $ (+v) <$> adj
      where
        dfs b s [] = go (b:bb) s
        dfs b s (v:vv)
            | S.member v s = dfs (b+1) (S.delete v s) $ map (+v) adj <> vv
            | otherwise = dfs b s vv
