-- https://adventofcode.com/2021/day/1

module A01 where

main = do
    xx <- getContents <&> map read . lines
    print $ count (<) xx
    print $ count (<) $ window 3 xx

delta f n = zipWith f <*> drop n

window n = delta (flip (-)) n . scanl (+) 0

count p = length . filter id . delta p 1
