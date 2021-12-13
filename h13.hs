module H13 where

import Data.List.Split (splitOn)
import Linear.V2
import qualified Data.Set as S

data Dir = X | Y deriving (Eq,Ord,Show)

tr = map f where f c = if isDigit c then c else ' '
readInts = map read . words . tr

vmax (V2 a b) (V2 c d) = V2 (max a c) (max b d)

main = do
    [rawDots,rawFolds] <- splitOn "\n\n" <$> getContents
    let dots = S.fromList $ parseDot <$> lines rawDots
    let folds = parseFold <$> lines rawFolds
    printf "Part 1: %d\n" (S.size . crease dots $ head folds)
    let final = foldl' crease dots folds
    let V2 mx my = foldl' vmax (V2 0 0) $ S.elems final
    printf "Part 2:\n"
    sequence_
        [ print
          [ if S.member (V2 x y) final then '#' else '.'
          | x <- [0..mx] ]
        | y <- [0..my] ]

parseDot = f . readInts where
    f [x,y] = V2 x y

parseFold s
    | elem 'x' s = V2 i 0
    | otherwise  = V2 0 i
  where
    [i] = readInts s

crease dots v = S.fromList $
    abs . (v -) . abs . (v -) <$> S.elems dots
