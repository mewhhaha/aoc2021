module Advent1 where

readNumbers :: IO [Int]
readNumbers = fmap read . lines <$> readFile "./data/advent1.txt"

{-
>>> solve1 [199,200,208,210,200,207,240,269,260,263] == 7
True

-}
solve1 :: [Int] -> Int
solve1 numbers = sum [1 | (prev, next) <- zip numbers (drop 1 numbers), prev < next]

run1 :: IO ()
run1 = do
  numbers <- readNumbers
  print $ solve1 numbers

{-
>>> solve2 [199,200,208,210,200,207,240,269,260,263] == 5
True

-}
solve2 :: [Int] -> Int
solve2 = solve1 . go
  where
    go (x : y : z : rest) = x + y + z : go (y : z : rest)
    go _ = []

run2 :: IO ()
run2 = do
  numbers <- readNumbers
  print $ solve2 numbers
