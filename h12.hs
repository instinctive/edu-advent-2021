{-# LANGUAGE OverloadedStrings #-}

module H12 where

import qualified Data.Map.Strict as M
import qualified Data.Text as T

main = do
    graph <- parse <$> getContents
    print $ solve graph

parse = M.fromListWith (<>) . concatMap (mk . pline) . lines where
    pline = map T.pack . words . map tr
    mk [a,b] = [(a,[b]),(b,[a])]
    tr '-' = ' '
    tr c = c

isSmall = isLower . flip T.index 0

solve graph = dfs 0 [["start"]] where
    dfs !n [] = n
    dfs !n (p@(x:xx):more)
        -- | traceShow p False = undefined
        | x == "end" = dfs (n+1) more
        | isSmall x && elem x xx = dfs n more
        | otherwise = dfs n $ next <> more
      where
        next = (:p) <$> M.findWithDefault [] x graph

