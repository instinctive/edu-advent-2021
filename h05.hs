module H05 where

import Linear.V2
import qualified Data.Map.Strict as M

type Pt = V2 Int
type Vent = (Pt,Pt)

main :: IO ()
main = do
    vents <- getContents <&> map parse . lines
    printf "Part 1: %d\n" (solve ortho vents)
    printf "Part 2: %d\n" (solve (const True) vents)

parse :: String -> Vent
parse = 
    mk . map read . words . map tr
  where
    mk [a,b,c,d] = (V2 a b, V2 c d)
    tr c = if isDigit c then c else ' '

solve :: (Vent -> Bool) -> [Vent] -> Int
solve test =
    M.size . 
    M.filter (>1) .
    M.fromListWith (+) . map (,1) .
    concatMap expand .
    filter test

ortho :: Vent -> Bool
ortho (V2 a b, V2 c d) = a == c || b == d

expand :: Vent -> [Pt]
expand (a,b) = 
    take (n+1) $ iterate (+d) a
  where
    V2 x y = b - a
    n = max (abs x) (abs y)
    d = V2 (div x n) (div y n)
