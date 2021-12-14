module H14 where

import Control.Monad.State.Strict (execState,modify)
import qualified Data.Map.Strict as M

main = do
    temp <- getLine
    void getLine
    rules <- getContents <&> M.fromList . map parse . lines
    printf "Part 1: %d\n" (solve rules temp 10 :: Int)
    printf "Part 2: %d\n" (solve rules temp 40 :: Int)

parse = f . words where f [[a,b],_,[c]] = ((a,b),c)

solve rules temp n =
    maximum counts - minimum counts
  where
    counts = M.elems . flip execState M.empty . go $ (,n) <$> temp
    add c = modify $ M.insertWith (+) c 1
    go [(a,_)]            = add a >>          pure ()
    go ((a,0):more)       = add a >>          go more
    go ((a,_):(b,0):more) = add a >> add b >> go more
    go ((a,i):(b,j):more) = go ((a,i):(c,k):(b,j):more) where
        c = rules M.! (a,b)
        k = min i j - 1
