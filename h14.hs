module H14 where

import Control.Monad.State.Strict ( evalState, modify', gets )
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

counts rules n a b =
    flip evalState M.empty $ memo (n,a,b)
  where
    memo k = gets (M.lookup k) >>= \case
        Just v -> pure v
        Nothing -> do
            v <- calc k
            modify' $ M.insert k v
            pure v
    calc (0,_,_) = pure MM.empty
    calc (i,a,b) = do
        let c = rules M.! (a,b)
        ac <- memo (i-1,a,c)
        cb <- memo (i-1,c,b)
        pure $ mconcat [ac,cb,single c]
