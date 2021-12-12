module H11 where

import Control.Monad.Extra
import Linear.V2
import qualified Data.Map.Strict as M

main = do
    start <- parse <$> getContents
    let z = M.map (const 0) start
    let reset m = M.union m z
    let seq = iterate (step . reset) start
    printf "Part 1: %d\n" $ part1 100 (M.size start) seq
    printf "Part 2: %d\n" $ part2 seq
  where
    step = fromLeft undefined . iterateM flash . M.map succ

parse raw = M.fromList
    [ (V2 r c, digitToInt i)
    | (r,s) <- zip [0..] $ lines raw
    , (c,i) <- zip [0..] s ]

part1 n z mm = n * z - calc mm where
    calc = sum . take n . tail . map M.size

part2 = fromJust . findIndex ((==0).M.size)

flash prev
    | M.null over = Left under
    | otherwise = Right $ foldl' bump under keys
  where
    (over,under) = M.partition (>9) prev
    keys = concatMap orbit (M.keys over)
    bump m k = M.adjust succ k m

orbit = let dd = V2 <$> [-1..1] <*> [-1..1] in
    \v -> (+v) <$> dd

idempotent f xx =
    getAlt . mconcat . zipWith go xx $ tail xx
  where
    go a b = Alt $ if f a == f b then Just a else Nothing
