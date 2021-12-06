{-# LANGUAGE ViewPatterns #-}

module Advent6 where

import Data.Bool
import Data.List

type Input = [(Int, Int)]

split :: Eq a => a -> [a] -> [[a]]
split delimiter = fmap (\f -> f []) . reverse . go []
  where
    go acc [] = acc
    go as (x : xs)
      | x == delimiter = go (id : as) xs
      | otherwise = case as of
        [] -> go [(x :)] xs
        (a : as) -> go ((a . (x :)) : as) xs

parseFish :: [String] -> [(Int, Int)]
parseFish = fmap ((,) <$> read . head <*> length) . group . sort

readInput :: IO Input
readInput = parseFish . split ',' <$> readFile "./data/advent6.txt"

run f = readInput >>= print . f

-- This solution is taken from https://www.reddit.com/r/haskell/comments/r9z4qb/advent_of_code_2021_day_06/hnfim87/?utm_source=reddit&utm_medium=web2x&context=3
generation :: Int -> [Int]
generation nTimer = replicate (nTimer + 1) 1 ++ zipWith (+) generation6 generation8

generation6 :: [Int]
generation6 = generation 6

generation8 :: [Int]
generation8 = generation 8

{-
>>>  5934 == solve1 (parseFish ["3","4","3","1","2"])
True

-}

solve1 :: Input -> Int
solve1 = sum . fmap (\(timer, fish) -> fish * generation timer !! 80)

run1 :: IO ()
run1 = run solve1

{-
>>>  solve2 (parseFish ["3","4","3","1","2"])
26984457539

-}

solve2 :: Input -> Int
solve2 = sum . fmap (\(timer, fish) -> fish * generation timer !! 256)

run2 :: IO ()
run2 = run solve2
