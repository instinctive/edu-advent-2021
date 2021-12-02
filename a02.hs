-- https://adventofcode.com/2021/day/2

{-# LANGUAGE TemplateHaskell #-}

module A02 where

import Control.Lens
import Control.Monad.State

data Sub = Sub { _subP, _subD, _subA :: !Int } deriving Show
makeLenses ''Sub
zSub = Sub 0 0 0
answer Sub{..} = _subP * _subD

main = do
    kk <- getContents <&> lines
    let (one,two) = solve kk
    print $ answer one
    print $ answer two

solve = flip execState (zSub,zSub) . sequence_ . map (parse.words)

parse [k,s] = let v = read @Int s in case k of
    "forward" -> do
        _1.subP += v
        _2.subP += v
        a <- use $ _2.subA
        _2.subD += v * a
    "down" -> do
        _1.subD += v
        _2.subA += v
    "up" -> do
        _1.subD -= v
        _2.subA -= v
    _ -> error k

parse e = error $ show e
