{-# LANGUAGE LambdaCase #-}

module Advent6 where

import Control.Monad.State.Strict
import Data.Bool
import Data.List
import qualified Data.Map as Map

type Input = [(Int, Int)]

{-
>>> split ',' "123,5,3"
["123","5","3"]
-}

split :: Eq a => a -> [a] -> [[a]]
split delimiter = (filter (/= delimiter) <$>) . groupBy (\_ b -> b /= delimiter)

parseFish :: [String] -> [(Int, Int)]
parseFish = fmap ((,) <$> read . head <*> length) . group . sort

readInput :: IO Input
readInput = parseFish . split ',' <$> readFile "./data/advent6.txt"

run f = readInput >>= print . f

generations :: Int -> [Int]
generations t = replicate (t + 1) 1 ++ zipWith (+) generations6 generations8

generations6 = generations 6

generations8 = generations 8

{-
>>>  solve1 (parseFish ["3","4","3","1","2"])
5934

-}

solve1 :: Input -> Int
solve1 = sum . fmap (\(t, n) -> n * generations t !! 80)

run1 :: IO ()
run1 = run solve1

{-
>>>  solve2 (parseFish ["3","4","3","1","2"])
26984457539

-}

solve2 :: Input -> Int
solve2 = sum . fmap (\(t, n) -> n * generations t !! 256)

run2 :: IO ()
run2 = run solve2
