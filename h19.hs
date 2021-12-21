module H19 where

import Data.List.Split ( splitOn )
import Linear.V3
import qualified Data.Array as A
import qualified Data.Set as S

main = do
    rawsegs <- splitOn "\n\n" <$> getContents
    let segs = parse . lines <$> rawsegs
    print $ solve segs
    traverse_ print
        [ ((i,j), sum . abs . fst <$> match s t)
        | (i,(s:ss)) <- zip [0..] (tails segs)
        , (j,t) <- zip [i+1..] ss ]
    -- traverse_ print $ map (second fst) $ solve segs

parse :: [String] -> [V3 Int]
parse (_:ss) = mk . f <$> ss where
    f = map read . words . map tr
    mk [a,b,c] = V3 a b c
    tr ',' = ' '
    tr c = c

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

add s xx = foldl' f s where
    f s v = flip S.insert s $ foldl' g v xx
    g v (delta,xfm) = xfm v + delta

match :: [Pt] -> [Pt] -> Maybe (Pt, Pt -> Pt)
match aa bb = go xfms where
    go [] = Nothing
    go (f:ff) = if length l >= 12 then Just (x,f) else go ff
      where
        xx = xlate f <$> aa <*> bb
        l@(x:_) = maximumBy (comparing length) . group . sort $ xx

solve segs =
    dfs (add S.empty [] $ seg 0) avail $ (0,[],) <$> S.elems avail
  where
    nsegs = length segs
    seg = let ary = A.listArray (0, nsegs - 1) segs in (ary A.!)
    avail = S.fromList [1..nsegs - 1]
    dfs q s _ | S.null s = S.size q
    dfs q s [] = error $ show s
    dfs q s ((i,xx,j):more)
        | S.notMember j s = dfs q s more
        | otherwise = case match (seg i) (seg j) of
            Nothing -> dfs q s more
            Just x ->
                let s' = S.delete j s in
                let xx' = x:xx in
                let more' = map (j,xx',) (S.elems s') in
                let q' = add q xx' (seg j) in
                dfs q' s' (more <> more')
