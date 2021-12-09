{-# LANGUAGE ViewPatterns #-}

module Advent9 where

import Data.Char (digitToInt)
import Data.Functor ((<&>))
import Data.List (foldl', sort, sortBy, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Ord (Down (Down), comparing)
import qualified Data.Set as Set

type Input = Map.Map (Int, Int) Int

readMap :: String -> Map.Map (Int, Int) Int
readMap = go 0 0 mempty
  where
    go _ _ acc [] = acc
    go x y acc ('\n' : ss) = go 0 (y + 1) acc ss
    go x y acc (s : ss) = go (x + 1) y (Map.insert (x, y) (digitToInt s) acc) ss

readInput :: FilePath -> IO Input
readInput path = readMap <$> readFile path

run f = readInput "./data/advent9.txt" >>= print . f

test f g = readInput "./data/advent9_test.txt" <&> g . f

adjacent (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

lowpoints :: Map.Map (Int, Int) Int -> [((Int, Int), Int)]
lowpoints m = filter lowpoint (Map.assocs m)
  where
    lowpoint (pos, v) = all (> v) $ mapMaybe (`Map.lookup` m) (adjacent pos)

{-
>>>  test solve1 id
15

-}

solve1 :: Input -> Int
solve1 = sum . fmap (succ . snd) . lowpoints

run1 :: IO ()
run1 = run solve1

{-
>>>  test solve2 id
1134

-}

basins (Map.filter (/= 9) -> m) = (basin <$> Set.singleton <*> pure) . fst <$> lowpoints m
  where
    basin explored [] = explored
    basin explored (x : xs) =
      let possible = filter (`Map.member` m) (adjacent x)
          unexplored = filter (`Set.notMember` explored) possible
       in basin (Set.fromList unexplored <> explored) (unexplored <> xs)

sortDesc = sortOn Down

solve2 :: Input -> Int
solve2 = product . take 3 . sortDesc . fmap Set.size . basins

run2 :: IO ()
run2 = run solve2
