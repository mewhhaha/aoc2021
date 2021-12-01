module Advent1 where

readNumbers :: IO [Int]
readNumbers = fmap read . lines <$> readFile "./data/advent1.txt"

{-
>>> solve1 [199,200,208,210,200,207,240,269,260,263] == 7
True

-}
solve1 :: [Int] -> Int
solve1 = go 0
  where
    go n (x : rest@(y : _)) = go (if x < y then n + 1 else n) rest
    go n _ = n

run1 :: IO ()
run1 = do
  numbers <- readNumbers
  print $ solve1 numbers

{-
>>> solve2 [199,200,208,210,200,207,240,269,260,263] == 5
True

-}
solve2 :: [Int] -> Int
solve2 = go 0
  where
    go n (x : rest@(_ : _ : w : _)) = go (if x < w then n + 1 else n) rest
    go n _ = n

run2 :: IO ()
run2 = do
  numbers <- readNumbers
  print $ solve2 numbers
