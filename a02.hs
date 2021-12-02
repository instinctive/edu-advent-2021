-- https://adventofcode.com/2021/day/2

{-# LANGUAGE TemplateHaskell #-}

module A02 where

import Control.Lens

data Sub = Sub { _subP, _subD, _subA :: !Int } deriving Show
makeLenses ''Sub
zSub = Sub 0 0 0
answer Sub{..} = _subP * _subD

main = do
    s <- getContents <&> lines
    print $ solve cmd1 s
    print $ solve cmd2 s

solve cmd s =
    answer $ foldl' (&) zSub $ map (parse cmd) s

parse cmd s = case words s of
    [k,x] -> cmd k (read x)
    _ -> error s

cmd1 = \case
    "forward" -> over subP . (+)
    "down"    -> over subD . (+)
    "up"      -> over subD . subtract
    e -> error e

cmd2 = \case
    "forward" -> \x s@Sub{..} -> s & over subP (+x) . over subD (+ (_subA * x))
    "down"    -> over subA . (+)
    "up"      -> over subA . subtract
    e -> error e
