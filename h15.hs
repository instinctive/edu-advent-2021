module H15 where

import Control.Monad.State.Strict (evalState,gets,modify',get)
import Linear.V2
import qualified Data.Array as A
import qualified Data.Map.Strict as M
import qualified Data.PQueue.Min as P

main = do
    ss <- lines <$> getContents
    let nr = length ss; nc = length (head ss)
    let final = V2 nr nc
    let terrain = parse final ss
    print $ show (nr,nc)
    -- let m = solve terrain 1 final
    -- sequence_
    --     [ sequence_
    --         [ printf " %4d" (M.findWithDefault (-1) (V2 r c) m)
    --         | c <- [1*nc-9..1*nc] ]
    --         >> putStr "\n"
    --     | r <- [1*nr-9..1*nr] ]
    putStr "Part 1: " >> print (solve terrain 1 final)
    putStr "Part 2: " >> print (solve terrain 5 final)

parse final ss =
    A.listArray (V2 1 1, final)
    [ digitToInt c | s <- ss, c <- s ]

type Pt = V2 Int

data Path = Path
    { _pLoc :: Pt
    , _pCost :: Int
    } deriving (Eq,Show)

data Stats = Stats { examined, pqmax, mapsz, cost :: Int } deriving Show

instance Ord Path where
    compare = comparing heur where heur Path{..} = _pCost - sum _pLoc

adj = let n = -1; nsew = [V2 1 0, V2 n 0, V2 0 1, V2 0 n] in
    \v -> (+v) <$> nsew

solve ary x final@(V2 nr nc) =
    flip evalState (M.singleton start 0) $
    go 0 0 (P.singleton $ Path start 0)
  where
    start = V2 1 1
    go z !n pq
        | _pLoc == V2 x x * final = gets M.size >>= \mz -> pure $ Stats n z mz _pCost
        | otherwise = do
            pp <- filterM cheaper next
            go (max z $ P.size pq) (n+1) $ foldl' (flip P.insert) q pp
      where
        Just (p@Path{..},q) = P.minView pq
        next = map mk . filter valid $ adj _pLoc
        mk v = Path v (_pCost + cost v)
        valid (V2 r c) = r >= 1 && c >= 1 && r <= x * nr && c <= x * nc
        cost (V2 r c) = mod (k-1) 9 + 1 where
            k = ary A.! (V2 (rr+1) (cr+1)) + rq + cq
            (rq,rr) = quotRem (r-1) nr
            (cq,cr) = quotRem (c-1) nc
        cheaper Path{..} = gets (M.lookup _pLoc) >>= \case
            Just c | c <= _pCost -> pure False
            _ -> modify' (M.insert _pLoc _pCost) >> pure True
