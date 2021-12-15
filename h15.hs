module H15 where

import Control.Monad.State.Strict (evalState,gets,modify')
import Linear.V2
import qualified Data.Array as A
import qualified Data.Map.Strict as M
import qualified Data.PQueue.Min as P

main = do
    ss <- lines <$> getContents
    let nr = length ss; nc = length (head ss)
    let final = V2 nr nc
    let terrain = parse final ss
    print $ solve terrain 1 final
    print $ solve terrain 5 final

parse final ss =
    A.listArray (V2 1 1, final)
    [ digitToInt c | s <- ss, c <- s ]

type Pt = V2 Int

data Path = Path
    { _pLoc :: Pt
    , _pCost :: Int
    } deriving (Eq,Show)

instance Ord Path where
    compare = comparing heur where heur Path{..} = _pCost - sum _pLoc

adj = let n = -1; nsew = [V2 1 0, V2 n 0, V2 0 1, V2 0 n] in
    \v -> (+v) <$> nsew

solve ary x final@(V2 nr nc) =
    flip evalState (M.singleton start 0) $
    go (P.singleton $ Path start 0)
  where
    start = V2 1 1
    cost (V2 r c) = mod (k-1) 9 + 1 where
        k = ary A.! (V2 (rr+1) (cr+1)) + rq + cq
        (rq,rr) = quotRem (r-1) nr
        (cq,cr) = quotRem (c-1) nc
    valid (V2 r c) = r >= 1 && c >= 1 && r <= x * nr && c <= x * nc
    cheaper Path{..} = gets (M.lookup _pLoc) >>= \case
        Just c | c <= _pCost -> pure False
        _ -> modify' (M.insert _pLoc _pCost) >> pure True
    go pq
        | _pLoc == V2 x x * final = pure _pCost
        | otherwise = do
            pp <- filterM cheaper next
            go $ foldl' (flip P.insert) q pp
      where
        Just (p@Path{..},q) = P.minView pq
        next = map mk . filter valid $ adj _pLoc
        mk v = Path v (_pCost + (cost v))
