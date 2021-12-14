module Advent7 where

import Data.Functor ((<&>))
import Data.List (groupBy)

type Input = [Int]

split :: Eq a => a -> [a] -> [[a]]
split d s = let (a, b) = break (== d) s in if null s then [] else a : split d (drop 1 b)

readInput :: FilePath -> IO Input
readInput path = fmap read . split ',' <$> readFile path

run f = readInput "./data/advent7.txt" >>= print . f

test f g = readInput "./data/advent7_test.txt" <&> g . f

{-
>>>  test solve1 (==37)
37

-}

solve1 :: Input -> Int
solve1 ps = minimum $ fuel <$> [minimum ps .. maximum ps]
  where
    fuel v = sum $ abs . subtract v <$> ps

run1 :: IO ()
run1 = run solve1

{-
>>>  test solve2 (==168)
True

-}

solve2 :: Input -> Int
solve2 ps = minimum $ fuel <$> [minimum ps .. maximum ps]
  where
    fuel v = sum $ cost . abs . subtract v <$> ps
    cost n = n * (n + 1) `div` 2

run2 :: IO ()
run2 = run solve2
