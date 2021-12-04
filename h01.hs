-- https://adventofcode.com/2021/day/1

module H01 where

main = do
    xx <- getContents <&> map read . lines
    let x3 = [ a+b+c | (a:b:c:_) <- tails xx ]
    answer 1 $ count xx
    answer 2 $ count x3

count s = length . filter id $ zipWith (<) s (tail s)

answer :: Show a => Int -> a -> IO ()
answer i a = printf "Part %d: " i >> print a
