module Advent1 where

type Input = [Int]

readInput :: IO Input
readInput = fmap read . lines <$> readFile "./data/advent1.txt"

run f = readInput >>= print . f

{-
>>> 7 == solve1 [199,200,208,210,200,207,240,269,260,263]
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
>>> 5 == solve2 [199,200,208,210,200,207,240,269,260,263]
True

-}
solve2 :: Input -> Int
solve2 = go 0
  where
    go n (x : rest@(_ : _ : w : _)) = go (if x < w then n + 1 else n) rest
    go n _ = n

run2 :: IO ()
run2 = run solve2
