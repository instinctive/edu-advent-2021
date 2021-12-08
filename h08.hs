module H08 where

import Data.Set ((\\))
import Prelude hiding ((\\))    
import qualified Data.Map.Strict as M
import qualified Data.Set as S

main = do
    probs <- getContents <&> map parse . lines
    let (one,two) = bimap sum sum . unzip $ solve <$> probs
    printf "Part 1: %d\n" one
    printf "Part 2: %d\n" two
  where
    parse = second tail . splitAt 10 . words

solve :: ([String],[String]) -> (Int,Int)
solve (obs',out') =
    (part1,part2)
  where
    part1 = length . filter p . map length $ out' where
        p x = x == 2 || x == 3 || x == 4 || x == 7
    part2 = foldl' go 0 out
    go n s = n * 10 + dict M.! s
    obs = S.fromList <$> obs'
    out = S.fromList <$> out'
    one   = head $ filterSize 2 obs
    four  = head $ filterSize 4 obs
    seven = head $ filterSize 3 obs
    eight = head $ filterSize 7 obs
    twothreefive = filterSize 5 obs
    zerosixnine  = filterSize 6 obs
    filterSize n = filter ((==n).S.size)
    top = seven \\ one
    botllft = eight \\ four \\ top
    botctr = foldl1' S.intersection twothreefive \\ top
    llft = botllft \\ botctr
    bot = botllft \\ llft
    ctr = botctr \\ bot
    two = head $ filter (S.isSubsetOf llft) twothreefive
    urgt = two \\ top \\ llft \\ bot \\ ctr
    lrgt = one \\ urgt
    ulft = eight \\ S.unions [top,llft,bot,ctr,urgt,lrgt]
    dict = M.fromList
        [ (eight \\ ctr,          0)
        , (one,                   1)
        , (two,                   2)
        , (eight \\ ulft \\ llft, 3)
        , (four,                  4)
        , (eight \\ urgt \\ llft, 5)
        , (eight \\ urgt,         6)
        , (seven,                 7)
        , (eight,                 8)
        , (eight \\ llft,         9)
        ]
