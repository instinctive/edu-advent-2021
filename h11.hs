module H11 where

import Control.Monad.Extra
import Linear.V2
import qualified Data.Map.Strict as M

main = do
    one <- solve 100 . parse <$> getContents
    printf "Part 1: %d\n" one
    -- printf "Part 2: %d\n" two

parse raw = M.fromList
    [ (V2 r c, digitToInt i)
    | (r,s) <- zip [0..] $ lines raw
    , (c,i) <- zip [0..] s ]

solve n m =
    M.size m * n - go m
  where
    go = sum . take n . tail . map M.size . iterate (step . reset)
    step = fromLeft undefined . iterateM flash . M.map succ
    reset q = M.union q z
    z = M.map (const 0) m

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
