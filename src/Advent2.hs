{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Advent2 where

import Control.Monad (msum)
import Data.Functor ((<&>))
import Data.List (foldl', inits, stripPrefix, tails)
import Data.Maybe (fromMaybe)

data Instruction = Forward Int | Down Int | Up Int

type Input = [Instruction]

splitOn :: Eq a => [a] -> [a] -> ([a], [a])
splitOn delimiter s = fromMaybe (s, []) . msum . (zipWith trySplit <$> inits <*> tails) $ s
  where
    trySplit a b = (a,) <$> stripPrefix delimiter b

parseInstruction :: String -> Instruction
parseInstruction (fmap (read @Int) . splitOn " " -> (command, n)) = case command of
  "forward" -> Forward n
  "down" -> Down n
  "up" -> Up n
  _ -> error "Panic!"

readInput :: FilePath -> IO Input
readInput path = fmap parseInstruction . lines <$> readFile path

run f = readInput "./data/advent2.txt" >>= print . f

test f g = readInput "./data/advent2_test.txt" <&> g . f

{-
>>> test solve1 (==150)
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
>>> test solve2 (==900)
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
