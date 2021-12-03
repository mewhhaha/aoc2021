{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Advent3 where

import Control.Arrow
import Data.Bifunctor (bimap)
import Data.Bits
import Data.Char (digitToInt)
import Data.Either (partitionEithers)
import Data.List (foldl')

toBits :: [String] -> [[Bool]]
toBits = (fmap . fmap) (== '1')

readBits :: IO [[Bool]]
readBits = toBits . lines <$> readFile "./data/advent3.txt"

number :: [Bool] -> Int
number = foldl' (\acc x -> acc * 2 + if x then 1 else 0) 0

{-
>>> solve1 (toBits ["00100","11110","10110","10111","10101","01111","00111","11100","10000","11001","00010","01010"]) == 198
True

-}

solve1 :: [[Bool]] -> Int
solve1 bits = ((*) <$> epsilon <*> gamma) . number . clamp . foldl' count (repeat 0) $ bits
  where
    bitlen = length . head $ bits
    bitmask = number $ replicate bitlen True
    count = zipWith (\acc x -> if x then acc + 1 else acc - 1)
    clamp = fmap (> 0)

    epsilon = id
    gamma = (.&. bitmask) . complement

run1 :: IO ()
run1 = do
  instructions <- readBits
  print $ solve1 instructions

{-
>>> solve2 (toBits ["00100","11110","10110","10111","10101","01111","00111","11100","10000","11001","00010","01010"]) == 230
True

-}

solve2 :: [[Bool]] -> Int
solve2 = (*) <$> ogr <*> co2
  where
    pickBy f cmp (os, zs) = if f os `cmp` f zs then os else zs

    calc :: (Int -> Int -> Bool) -> [[Bool]] -> Int
    calc cmp bits = go (0, bits)
      where
        go (i, [x]) = number x
        go (i, xs) =
          let pass = pickBy length cmp . partitionEithers . fmap (\n -> if n !! i then Left n else Right n)
           in go (i + 1, pass xs)

    ogr = calc (>=)
    co2 = calc (<)

run2 :: IO ()
run2 = do
  instructions <- readBits
  print $ solve2 instructions
