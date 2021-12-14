module H14 where

import qualified Data.Map.Strict as M

main = do
    temp <- getLine
    void getLine
    rules <- getContents <&> M.fromList . map parse . lines
    let (one,two) = solve rules temp :: (Int,Int)
    printf "Part 1: %d\n" one
    printf "Part 2: %d\n" two

parse = f . words where f [[a,b],_,[c]] = ([a,b],[c,b])

solve m s =
    (at 10, at 40)
  where
    at i = let xx = count i in maximum xx - minimum xx
    count i = M.elems . M.fromListWith (+) . map (,1) $ ss !! i
    ss = iterate (step m) s

step m s = (head s:) . concat $ zipWith f s (tail s) where
    f a b = M.findWithDefault [b] [a,b] m
