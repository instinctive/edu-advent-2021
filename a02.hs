-- https://adventofcode.com/2021/day/2

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module A02 where

import Control.Lens
import Data.String.QQ

data Sub = Sub { _subH, _subD, _subA :: !Int } deriving Show
makeLenses ''Sub
zSub = Sub 0 0 0
answer Sub{..} = _subH * _subD

main = do
    s <- getContents <&> lines
    print $ solve cmd1 s
    print $ solve cmd2 s

solve cmd s =
    flip (,) <*> answer $
    foldl' (&) zSub $ map (parse cmd) s

parse cmd s = case words s of
    [k,x] -> cmd k (read x)
    _ -> error s

cmd1 = \case
    "forward" -> over subH . (+)
    "down"    -> over subD . (+)
    "up"      -> over subD . subtract
    e -> error e

cmd2 = \case
    "forward" -> \x s@Sub{..} -> s & over subH (+x) . over subD (+ (_subA * x))
    "down"    -> over subA . (+)
    "up"      -> over subA . subtract
    e -> error e

test :: String
test = [s|
forward 5
down 5
forward 8
up 3
down 8
forward 2
|]
