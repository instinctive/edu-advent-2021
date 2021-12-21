module H20 where

import Linear.V2
import qualified Data.Array as A
import qualified Data.Set as S

main = do
    alg <- getLine
    void getLine
    input <- lines <$> getContents
    print $ solve alg input

pinput ss = S.fromList
    [ V2 r c
    | (r,s) <- zip [0..] ss
    , (c,x) <- zip [0..] s
    , x == '#' ]

palg = A.listArray (0,511) . map f where
    f '#' = True
    f '.' = False

solve rawalg rawimg =
    S.size $ images !! 25
  where
    alg = let ary = palg rawalg in (ary A.!)
    img = pinput rawimg
    images = iterate (enhance not alg . enhance id alg) img

adj v = map (+v) $ V2 <$> [-1..1] <*> [-1..1]

enhance b alg img =
    S.fromList $ mapMaybe calc cands
  where
    cands = S.elems . S.fromList . concatMap adj $ S.elems img
    calc v = if not . b . alg $ foldl' f 0 (adj v)
        then Just v
        else Nothing
    f i v = i * 2 + if (b $ S.member v img) then 1 else 0
