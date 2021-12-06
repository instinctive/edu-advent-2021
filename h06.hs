module H06 where

import Data.Array ((!),(//))
import qualified Data.Array as A
import qualified Data.Map.Strict as M

main :: IO ()
main = do
    ary <- parse <$> getContents
    printf "Part 1: %d\n" $ solve 80  (0,ary)
    printf "Part 2: %d\n" $ solve 256 (0,ary)

solve n =
    sum . snd . (!!n) . iterate f
  where
    plus a b = mod (a + b) 9
    f (z,ary) = (plus z 1, ary') where
        ary' = ary // [(plus z 7, (ary ! (plus z 7)) + (ary ! z))]

parse :: String -> A.Array Int Int
parse =
    A.listArray (0,8) .
    M.elems .
    M.fromListWith (+) .
    ( map (,0) [0..8] <> ) .
    map (,1) . map read . words . map tr 
  where
    tr c = if isDigit c then c else ' '
