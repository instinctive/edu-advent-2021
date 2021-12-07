module H07 where

import qualified Data.Map.Strict as M

main :: IO ()
main = do
    xx <- getContents <&> parse
    let (one,two) = solve xx
    printf "Part 1: %d\n" one
    printf "Part 2: %d\n" two

parse = map (read @Int) . words . map (bool ' ' <*> isDigit)

solve :: [Int] -> (Int,Int)
solve xx =
    (answer 2, answer 3)
  where
    yy = xx & go . M.assocs . M.fromListWith (+) . map (,1) where
        go [(_,x)] = [x]
        go ((i,x):more@((j,_):_)) = [x] <> replicate (j - i - 1) 0 <> go more
    calc = iterate (scanl1 (+))
    fwd = yy & calc . (0:)
    bwd = yy & calc . (0:) . reverse
    answer i = minimum $ zipWith (+) 
        (                 fwd !! i) 
        (tail . reverse $ bwd !! i)
