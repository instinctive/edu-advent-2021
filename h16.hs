module H16 where

import Data.List.Split ( chunksOf )

main = do
    bits <- lines <$> getContents
    -- traverse_ print bits
    traverse_ (go . concatMap parseHex) bits
  where
    parseHex :: Char -> String
    parseHex = printf "%04b" . digitToInt
    go s = do
        -- print s
        let Just (p,_) = packet s
        -- print p
        putStr "Part 1: " >> print (psum p)
        putStr "Part 2: " >> print (head $ pval p)

bin = foldl' f 0 where f n c = n * 2 + digitToInt c

parse n = first bin . splitAt n

data Packet = Packet
    { _pVersion  :: Int
    , _pType     :: Int
    , _pContents :: Contents
    } deriving Show

data Contents = Lit Int | Args [Packet] deriving Show

psum Packet{..} = _pVersion + csum _pContents
csum (Lit _) = 0
csum (Args pp) = sum $ psum <$> pp

pval Packet{..} = (:[]) $ case _pType of
    0 -> sum args
    1 -> product args
    2 -> minimum args
    3 -> maximum args
    4 -> head args
    5 -> if a > b then 1 else 0
    6 -> if a < b then 1 else 0
    7 -> if a == b then 1 else 0
  where
    [a,b] = args
    args = case _pContents of
        Lit x -> [x]
        Args pp -> concatMap pval pp

cval (Lit x) = x
cval (Args pp) = sum $ psum <$> pp

packet [] = Nothing
packet s  = Just (Packet v t c, cs) where
    (v,vs) = parse  3  s -- version
    (t,ts) = parse  3 vs -- type
    (i,is) = parse  1 ts -- length id
    (b,bs) = parse 15 is -- number of bits
    (p,ps) = parse 11 is -- number of packets
    (c,cs)
        | t == 4 = literal ts
        | i == 0 = first (Args . unfoldr packet) (splitAt b bs)
        | i == 1 = go p [] ps
    go 0 rr more = (Args $ reverse rr, more)
    go p rr more =
        let Just (r,more') = packet more in
        go (p-1) (r:rr) more'

literal s = (Lit lit, concat more) where
    (ones,zero:more) = span ((=='1').head) (chunksOf 5 s)
    lit = bin . concatMap tail $ ones <> [zero]
