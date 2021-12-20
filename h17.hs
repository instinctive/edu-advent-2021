module H17 where

import Linear.V2

main = do
    args <- parse <$> getContents :: IO [Int]
    putStr "Part 1: " >> print (part1 args)
    putStr "Part 2: " >> print (part2 args)
  where
    parse = map read . words . map tr
    tr c = if 
        | isDigit c -> c
        | c == '-'  -> '-'
        | otherwise -> ' '

part1 [xlo,xhi,ylo,yhi] = tri $ abs ylo - 1

part2 args@[xlo,xhi,ylo,yhi] =
    traceShow args $
    traceShow xx $
    traceShow yy $
    length . filter check . map path $ V2 <$> xx <*> yy
  where
    xx = dropWhile ((<xlo).tri) [1..xhi]
    yy = [ylo .. abs ylo - 1]
    check = any valid . takeWhile possible
    valid (V2 x y) = x >= xlo && y <= yhi
    possible (V2 x y) = x <= xhi && y >= ylo

path v = pp where
    pp = scanl (+) (V2 0 0) vv
    vv = iterate g v
    g (V2 x y) = V2 (max 0 $ x-1) (y-1)

tri n = div (n*n + n) 2
