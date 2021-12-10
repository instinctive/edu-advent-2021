module H10 where

main = do
    input <- getContents <&> lines
    let (one,two) = quux input
    printf "Part 1: %d\n" one
    printf "Part 2: %d\n" two
  where
    quux = bimap sum median . partitionEithers . map solve
    median xx = sort xx !! (length xx `div` 2)

solve :: String -> Either Int Int
solve = go " " where
    go ll "" = Right $ score ll
    go lll@(l:ll) (r:rr) = case linfo r of
        Nothing -> go (r:lll) rr
        Just (c,v) -> if c == l then go ll rr else Left v

linfo ')' = Just ('(', 3)
linfo ']' = Just ('[', 57)
linfo '}' = Just ('{', 1197)
linfo '>' = Just ('<', 25137)
linfo _ = Nothing

score = (`div` 5) . foldl' f 0 where
    f n c = n * 5 + points c

points ' ' = 0
points '(' = 1
points '[' = 2
points '{' = 3
points '<' = 4
