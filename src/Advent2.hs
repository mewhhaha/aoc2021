{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Advent2 where

import Data.List (foldl')

data Instruction = Forward Int | Down Int | Up Int

type Input = [Instruction]

splitBy :: Eq a => a -> [a] -> ([a], [a])
splitBy delimiter = go []
  where
    go first (x : rest)
      | x == delimiter = (reverse first, rest)
      | otherwise = go (x : first) rest
    go first [] = (reverse first, [])

readInstruction (fmap (read @Int) . splitBy ' ' -> (command, n)) = case command of
  "forward" -> Forward n
  "down" -> Down n
  "up" -> Up n
  _ -> error "Panic!"

readInput :: IO Input
readInput = fmap readInstruction . lines <$> readFile "./data/advent2.txt"

run f = readInput >>= print . f

{-
>>> 150 == solve1 [Forward 5,Down 5,Forward 8,Up 3,Down 8,Forward 2]
True

-}
solve1 :: Input -> Int
solve1 = uncurry (*) . foldl' go (0, 0)
  where
    go (position, depth) instruction = case instruction of
      (Forward x) -> (position + x, depth)
      (Down x) -> (position, depth + x)
      (Up x) -> (position, depth - x)

run1 :: IO ()
run1 = run solve1

{-
>>> 900 == solve2 [Forward 5,Down 5,Forward 8,Up 3,Down 8,Forward 2]
True

-}
solve2 :: Input -> Int
solve2 = uncurry (*) . fst . foldl' go ((0, 0), 0)
  where
    go ((position, depth), aim) instruction = case instruction of
      (Forward x) -> ((position + x, depth + (x * aim)), aim)
      (Down x) -> ((position, depth), aim + x)
      (Up x) -> ((position, depth), aim - x)

run2 :: IO ()
run2 = run solve2