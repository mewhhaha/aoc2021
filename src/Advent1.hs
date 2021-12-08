module Advent1 where

import Data.Functor ((<&>))

type Input = [Int]

readInput :: FilePath -> IO Input
readInput path = fmap read . lines <$> readFile path

run f = readInput "./data/advent1.txt" >>= print . f

test f g = readInput "./data/advent1_test.txt" <&> g . f

{-
>>> test solve1 (==7)
True

-}
solve1 :: Input -> Int
solve1 = go 0
  where
    go n (x : rest@(y : _)) = go (if x < y then n + 1 else n) rest
    go n _ = n

run1 :: IO ()
run1 = run solve1

{-
>>> test solve2 (==5)
True

-}
solve2 :: Input -> Int
solve2 = go 0
  where
    go n (x : rest@(_ : _ : w : _)) = go (if x < w then n + 1 else n) rest
    go n _ = n

run2 :: IO ()
run2 = run solve2
