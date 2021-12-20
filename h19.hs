module H19 where

import Data.List.Split ( splitOn )
import Linear.V3
import qualified Data.Map.Strict as M

main = do
    rawsegs <- splitOn "\n\n" <$> getContents
    let segs = parse . lines <$> rawsegs
    traverse_ print
        [ ((i,j), fst <$> match s t)
        | (i,(s:ss)) <- zip [0..] (tails segs)
        , (j,t) <- zip [i+1..] ss ]

parse :: [String] -> [V3 Int]
parse (_:ss) = mk . f <$> ss where
    f = map read . words . map tr
    mk [a,b,c] = V3 a b c
    tr ',' = ' '
    tr c = c

solve _ = 0 :: Int

type Pt = V3 Int

rots :: [Pt -> Pt]
rots =
    [ \(V3 x y z) -> V3 x   y    z
    , \(V3 x y z) -> V3 x (-z)   y
    , \(V3 x y z) -> V3 x (-y) (-z)
    , \(V3 x y z) -> V3 x   z  (-y) ]

oris :: [Pt -> Pt]
oris =
    [ \(V3 x y z) -> V3 x y z
    , \(V3 x y z) -> V3 z x y
    , \(V3 x y z) -> V3 y z x
    , \(V3 x y z) -> V3 (-x) (-y) z
    , \(V3 x y z) -> V3 (-z) (-x) y
    , \(V3 x y z) -> V3 (-y) (-z) x ]

xfms :: [Pt -> Pt]
xfms = (.) <$> rots <*> oris

xlate :: (Pt -> Pt) -> Pt -> Pt -> Pt
xlate xfm origin cand = 
    let dest = xfm cand in
    origin - dest

match :: [Pt] -> [Pt] -> Maybe (Pt, Pt -> Pt)
match aa bb = go xfms where
    go [] = Nothing
    go (f:ff) = if length l >= 12 then Just (x,f) else go ff
      where
        xx = xlate f <$> aa <*> bb
        l@(x:_) = maximumBy (comparing length) . group . sort $ xx
