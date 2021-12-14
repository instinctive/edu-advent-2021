module H14 where

import Control.Monad.State.Strict (execState,modify)
import qualified Data.Map.Monoidal.Strict as MM
import qualified Data.Map.Strict as M

type Map = MM.MonoidalMap

main = do
    temp <- getLine
    void getLine
    rules <- getContents <&> M.fromList . map parse . lines
    printf "Part 1: %d\n" (solve rules temp 10 :: Int)
    printf "Part 2: %d\n" (solve rules temp 40 :: Int)

parse = f . words where f [[a,b],_,[c]] = ((a,b),c)

single :: Char -> Map Char (Sum Int)
single c = MM.singleton c 1

solve rules temp n = getSum $
    maximum xx - minimum xx
  where
    start = (,n) <$> temp
    inner = mconcat $ zipWith (counts rules) start (tail start)
    outer = mconcat $ single <$> temp
    xx = MM.elems $ inner <> outer

counts rules = go where
    go ai@(a,i) bj@(b,j)
        | i == 0 || j == 0 = MM.empty
        | otherwise = go ai ck <> go ck bj <> single c
      where
        ck@(c,_) = (rules M.! (a,b), min i j - 1)
