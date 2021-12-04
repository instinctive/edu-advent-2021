module H04 where

import Data.List.Split (splitOn)
import Data.Tagged (Tagged(..))
import qualified Data.Set as S
import qualified Data.Map.Monoidal.Strict as M

type Map = M.MonoidalMap

data Tags = Board | Draw | Time | Score
type Score = Tagged 'Score Int
type Board = Tagged 'Board Int
type Draw = Tagged 'Draw Int -- a number drawn for bingo
type Time = Tagged 'Time Int

data Bingo = Row Int | Col Int deriving (Eq,Ord,Show)

main :: IO ()
main = do
    xx' : bb' <- getContents <&> splitOn "\n\n"
    let xx = Tagged . read <$> splitOn "," xx' :: [Draw]
    let bb = mconcat $ zipWith mkboard [1..] bb'
    let uu = M.fromListWith S.union $ concatMap mk $ M.assocs bb
    let ans = solve M.empty M.empty uu bb xx
    answer 1 $ head ans
    answer 2 $ last ans
  where
    mk (i,qq) = (,S.singleton i) <$> map fst qq

answer :: Int -> (Score,Board) -> IO ()
answer i (Tagged v,Tagged b) = printf "Part %d: #%d = %d\n" i b v

mkboard :: Board -> String -> Map Draw [(Board,Bingo)]
mkboard b s = mconcat
    [ M.singleton x [(b,Row r),(b,Col c)]
    | (r,line) <- zip [0..] $ lines s
    , (c,word) <- zip [0..] $ words line
    , let x = Tagged (read word)
    ]

solve ::
     Map Board (Time,Score)   -> -- time a board bingo'd and its score
     Map (Board,Bingo) Int    -> -- number of hits on a bingo line
     Map Board (S.Set Draw)   -> -- unused draws for a board
     Map Draw [(Board,Bingo)] -> -- the bingo lines for a draw
     [Draw]                   -> -- the sequence of draws
     [(Score, Board)]            -- the board scores in increasing time ordek
solve done _ _ _ [] = 
    map (first snd) . sort $ 
    swap <$> M.assocs done
solve done used unused avail (x:xx) =
    solve done' used' unused' avail' xx
  where
    qq = fromJust $ M.lookup x avail                             -- bingo lines for this draw
    avail' = M.delete x avail                                    -- can't reuse this draw
    used' = foldl' f used qq where                               -- bump the bingo line count
        f m q = M.insertWith (+) q 1 m                           --
    unused' = foldl' f unused $ fst <$> qq where                 -- draw is no longer unused
        f m b = M.adjust (S.delete x) b m                        --
    bingo = S.fromList . map fst . M.keys $ M.filter (==5) used' -- boards with bingos
    newbingo = S.difference bingo $ M.keysSet done               -- only the new ones
    done' = foldl' f done $ quux <$> S.elems newbingo where      -- add them to done
        f m (b,v) = M.insert b v m                               --
    quux b = (b, (coerce $ M.size done, calc b))                 -- use size as time proxy
    calc b = coerce $ x * sum                                    -- score calculation
        (S.elems $ fromJust $ M.lookup b unused')
