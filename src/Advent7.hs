module Advent7 where

import Data.List (groupBy)

type Input = [Int]

split :: Eq a => a -> [a] -> [[a]]
split delimiter = (filter (/= delimiter) <$>) . groupBy (\_ b -> b /= delimiter)

readInput :: IO Input
readInput = fmap read . split ',' <$> readFile "./data/advent7.txt"

run f = readInput >>= print . f

{-
>>>  solve1 [16,1,2,0,4,2,7,1,2,14]
37

-}

solve1 :: Input -> Int
solve1 positions = minimum $ fuel <$> [low .. high]
  where
    fuel v = sum $ abs . subtract v <$> positions

    low = minimum positions
    high = maximum positions

run1 :: IO ()
run1 = run solve1

{-
>>>  solve2 [16,1,2,0,4,2,7,1,2,14]
168

-}

solve2 :: Input -> Int
solve2 positions = minimum $ fuel <$> [low .. high]
  where
    fuel v = sum $ sumSeries . abs . subtract v <$> positions

    low = minimum positions
    high = maximum positions
    sumSeries n = n * (n + 1) `div` 2

run2 :: IO ()
run2 = run solve2
