{-# LANGUAGE OverloadedStrings #-}

module H12 where

import qualified Data.Map.Strict as M
import qualified Data.Text as T

main = do
    graph <- parse <$> getContents
    printf "Part 1: %d\n" $ solve True graph
    printf "Part 2: %d\n" $ solve False graph

parse = M.fromListWith (<>) . filter valid . concatMap (mk . pline) . lines where
    pline = map T.pack . words . map tr
    valid (_,[a]) = a /= "start"
    mk [a,b] = [(a,[b]),(b,[a])]
    tr '-' = ' '
    tr c = c

isSmall = isLower . flip T.index 0

solve b graph = dfs (0::Int) [(b,["start"])] where
    dfs !n [] = n
    dfs !n ((used,p@(x:xx)):more)
        | x == "end" = dfs (n+1) more
        | used && visited = dfs n more
        | otherwise = dfs n $ next <> more
      where
        visited = isSmall x && elem x xx
        next = (used || visited,).(:p) <$> M.findWithDefault [] x graph
