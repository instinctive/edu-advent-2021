module H22 where

import Control.Lens
import Control.Monad.ST ( runST )
import Data.Array ( listArray, bounds )
import Data.Array.ST ( STUArray, newArray, writeArray, readArray )
import Linear.V3
import qualified Data.Array as A
import qualified Data.IntMap.Strict as M
import qualified Data.Set as S

type Pt = V3 Int
type Cube = (Int,(Pt,Pt))

main = do 
    cubes <- map parse . lines <$> getContents
    -- traverse_ print cubes
    -- traverse_ print $ solve cubes
    print $ solve cubes

parse :: String -> Cube
parse s = (on st,(lo,hi)) where
    [st,coords] = words s
    on "on" = 1
    on "off" = 0
    [xlo,xhi,ylo,yhi,zlo,zhi] = map read . words $ map tr coords
    tr '-' = '-'
    tr c = if isDigit c then c else ' '
    lo = V3 xlo ylo zlo
    hi = V3 xhi yhi zhi + 1 -- not included

coord l cc = (xmap,xary) where
    xx = S.elems . S.fromList $ cc ^.. each . _2 . each . l
    xmap = M.fromList (zip xx [0..])
    xary = listArray (0, length xx - 1) xx

solve cc = runST do
    ary <- newArray (vz,vx) 0 :: ST s (STUArray s Pt Int)
    for_ cc \(b,qq) -> 
        for_ (range . second (subtract 1) $ bimap xfm xfm qq) \i ->
            writeArray ary i b
    -- readArray ary vz
    let f !n i = readArray ary i >>= \x -> pure $ n + x * volume i
    -- foldM f 0 $ range part1'
    foldM f 0 $ range (vz,vx-1)
  where
    xfm (V3 x y z) = V3 (xmap M.! x) (ymap M.! y) (zmap M.! z)
    inv (V3 x y z) = V3 (xary A.! x) (yary A.! y) (zary A.! z)
    (xmap,xary) = coord _x $ (0,part1) : cc
    (ymap,yary) = coord _y $ (0,part1) : cc
    (zmap,zary) = coord _z $ (0,part1) : cc
    vz = V3 0 0 0
    vx = let f = snd . bounds in V3 (f xary) (f yary) (f zary)
    volume i = product $ inv (i + 1) - inv i
    part1 = let n = -50 in (V3 n n n, V3 51 51 51)
    part1' = bimap xfm xfm part1
