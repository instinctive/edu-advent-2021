module H14 where

-- import Control.Monad.State.Strict (execState,modify)
import qualified Data.Map.Monoidal.Strict as MM
import qualified Data.Map.Strict as M

type Map = MM.MonoidalMap

main = do
    start <- getLine
    void getLine
    rules <- getContents <&> M.fromList . map parse . lines
    printf "Part 1: %d\n" (solve rules start 10 :: Int)
    printf "Part 2: %d\n" (solve rules start 40 :: Int)

parse = f . words where f [[a,b],_,[c]] = ((a,b),c)

single :: Char -> Map Char (Sum Int)
single c = MM.singleton c 1

solve rules start n = getSum $
    maximum xx - minimum xx
  where
    inner = mconcat $ zipWith (counts rules n) start (tail start)
    outer = mconcat $ single <$> start
    xx = MM.elems $ inner <> outer

counts rules = go where
    go 0 _ _ = MM.empty
    go i a b = 
        let c = rules M.! (a,b) in
        go (i-1) a c <> go (i-1) c b <> single c
