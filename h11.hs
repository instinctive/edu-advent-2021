module H11 where

import Control.Monad.Extra
import Linear.V2
import qualified Data.Map.Strict as M

main = do
    start <- parse <$> getContents
    let (one,two) = solve start
    printf "Part 1: %d\n" one
    printf "Part 2: %d\n" two
  where

parse raw = M.fromList
    [ (V2 r c, digitToInt i)
    | (r,s) <- zip [0..] $ lines raw
    , (c,i) <- zip [0..] s ]

solve start =
    (100 * n - one seq, two seq)
  where
    one = sum . take n . tail . map M.size
    two = fromJust . findIndex ((==0).M.size)
    seq = iterate (step . reset) start
    step = fromLeft undefined . iterateM flash . M.map succ
    reset m = M.union m z
    z = M.map (const 0) start
    n = M.size start

flash prev
    | M.null over = Left under
    | otherwise = Right $ foldl' bump under keys
  where
    (over,under) = M.partition (>9) prev
    keys = concatMap orbit (M.keys over)
    bump m k = M.adjust succ k m

orbit = let dd = V2 <$> [-1..1] <*> [-1..1] in
    \v -> (+v) <$> dd
