{-# LANGUAGE TupleSections #-}

module Advent3 where

import Data.Bool (bool)
import Data.Either (partitionEithers)
import Data.Functor ((<&>))
import Data.List (foldl', transpose)

type Input = [[Bool]]

parseBits :: [String] -> [[Bool]]
parseBits = (fmap . fmap) (== '1')

readInput :: FilePath -> IO Input
readInput path = parseBits . lines <$> readFile path

run f = readInput "./data/advent3.txt" >>= print . f

test f g = readInput "./data/advent3_test.txt" <&> g . f

number :: [Bool] -> Int
number = foldl' (\acc x -> acc * 2 + if x then 1 else 0) 0

{-
>>> test solve1 (==198)
True

-}

solve1 :: Input -> Int
solve1 =
  ((*) <$> epsilon <*> gamma)
    . fmap ((> 0) . count)
    . transpose
  where
    count = foldl' (\acc bit -> acc + bool 1 (-1) bit) 0
    epsilon = number
    gamma = number . fmap not

run1 :: IO ()
run1 = run solve1

{-
>>> test solve2 (==230)
True

-}

solve2 :: Input -> Int
solve2 = (*) <$> ogr <*> co2
  where
    pick cmp (os, zs) = if length os `cmp` length zs then os else zs

    calc :: (Int -> Int -> Bool) -> [[Bool]] -> Int
    calc cmp = go . (0,)
      where
        go (i, [n]) = number n
        go (i, ns) =
          let categorize f n = if f n then Left n else Right n
              pass = pick cmp . partitionEithers . fmap (categorize (!! i))
           in go (i + 1, pass ns)

    ogr = calc (>=)
    co2 = calc (<)

run2 :: IO ()
run2 = run solve2
