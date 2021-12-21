module H21 where

import Control.Monad.Trans ( lift )
import Control.Monad.State ( evalStateT, get, gets, put )

main = do
    [_,p1] <- parse
    [_,p2] <- parse
    solve p1 p2
  where
    parse :: IO [Int]
    parse = map read . words . map tr <$> getLine
    tr c = if isDigit c then c else ' '

solve a b = do
    ans <- flip evalStateT (0,0) $ go 0 0 a b
    print ans
  where
    roll = get >>= \(z,n) -> put (mod (z+1) 10, n+1) >> pure (z+1)
    go s1 s2 p1 p2
        | s1 >= 1000 = gets snd >>= \n -> pure (s2 * n)
        | s2 >= 1000 = gets snd >>= \n -> pure (s1 * n)
        | otherwise = do
            a <- roll
            b <- roll
            c <- roll
            let p = mod (p1 + a + b + c - 1) 10 + 1
            let s = s1 + p
            go s2 s p2 p
