module H18 where

data Snail a = LBrace | RBrace | Item a deriving (Eq,Ord,Show)

main = do
    snails <- map parse . lines <$> getContents
    let final = foldl1' add snails
    print $ render final
    print $ mag final
    print $ maximum
        [ m -- traceShow (m, render c) m
        | a <- snails
        , b <- snails
        , a /= b
        , let c = add a b
        , let m = mag c
        ]

mag = go [] where
    go [m] [] = m
    go (a:b:more) (RBrace:s) = 
        let c = 2*a + 3*b in
        -- traceShow (a,b,c) $
        go (c : more) s
    go mm (Item i : s) = go (i : mm) s
    go mm (LBrace : s) = go mm s

test = mag . parse

parse = \case
    "" -> []
    '[' : s -> LBrace : parse s
    ']' : s -> RBrace : parse s
    ',' : s -> parse s
    s -> let (dd,s') = span isDigit s in
        Item (read dd) : parse s'

render :: [Snail Int] -> String
render = \case
    [] -> ""
    LBrace : Item i : s -> "[" <> show i <> "," <> render s
    RBrace : LBrace : s -> "]," <> render (LBrace : s)
    LBrace : s -> '[' : render s
    RBrace : s -> ']' : render s
    Item i : s -> show i <> render s

add a b = 
    -- traceShow ("add ",render a) $
    -- traceShow ("add ",render b) $
    -- traceShow ("sum ",render c) $
    c
  where
    c = explode $ [LBrace] <> a <> b <> [RBrace]

explode = go False [] 0 where
    -- go b rr n ff | traceShow (b,render rr,n,render ff) False = undefined
    go b rr _ [] = split [] 0 $ reverse rr
    go _ rr 4 (LBrace : Item a : Item b : RBrace : ff) =
        go True (plus a rr) 4 (Item 0 : plus b ff)
    go b rr 4 err@(LBrace : _) = error $ show err
    go b rr n (f : ff) = go b (f : rr) (depth f n) ff

    split rr _ [] = reverse rr
    split rr n (Item i : ff) | i > 9 =
        go False rr n (LBrace : Item (div i 2) : Item (div (i+1) 2) : RBrace : ff)
    split rr n (f : ff) = split (f : rr) (depth f n) ff

depth LBrace = succ
depth RBrace = pred
depth (Item _) = id

plus i xx = case bb of
    [] -> xx
    (Item j : cc) -> aa <> (Item (i + j) : cc)
  where
    (aa,bb) = break isItem xx
    isItem (Item _) = True
    isItem _ = False
