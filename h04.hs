module H04 where

import Data.List.Split (splitOn)
import qualified Data.Set as S
import qualified Data.Map.Monoidal.Strict as M

main = do
    xx' : bb' <- getContents <&> splitOn "\n\n"
    let xx = read @Int <$> splitOn "," xx'
    let bb = mconcat $ zipWith mkboard [1..] bb'
    let uu = M.fromListWith S.union $ concatMap mk $ M.assocs bb
    let ans = solve M.empty M.empty uu bb xx
    answer 1 $ head ans
    answer 2 $ last ans
  where
    mk (i,qq) = (,S.singleton i) <$> map fst qq

answer :: Int -> ((Int,Int),Int) -> IO ()
answer i ((_,v),b) = printf "Part %d: #%d = %d\n" i b v

data Bingo = Row Int | Col Int deriving (Eq,Ord,Show)

mkboard b s = mconcat
    [ M.singleton x [(b,Row r),(b,Col c)]
    | (r,line) <- zip [0..] $ lines s
    , (c,word) <- zip [0..] $ words line
    , let x = read @Int word
    ]

solve done _ _ _ [] = sort $ swap <$> M.assocs done
solve done used unused avail (x:xx) =
    solve done' used' unused' avail' xx
  where
    qq = fromJust $ M.lookup x avail
    avail' = M.delete x avail
    used' = foldl' f used qq where
        f m q = M.insertWith (+) q 1 m
    unused' = foldl' f unused $ fst <$> qq where
        f m b = M.adjust (S.delete x) b m
    bingo = S.fromList . map fst . M.keys $ M.filter (==5) used'
    newbingo = S.difference bingo $ M.keysSet done
    done' = foldl' f done $ quux <$> S.elems newbingo where
        f m (b,v) = M.insert b v m
    quux b = (b, (M.size done, calc b))
    calc b = x * sum (S.elems $ fromJust $ M.lookup b unused')
