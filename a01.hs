-- https://adventofcode.com/2021/day/1

module A01 where

main = do
    xx <- getContents <&> map read . lines
    let x3 = [ a+b+c | (a:b:c:_) <- tails xx ]
    print $ count xx
    print $ count x3

count s = length . filter id $ zipWith (<) s (tail s)
