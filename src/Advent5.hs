{-# LANGUAGE TupleSections #-}

module Advent5 where

import Data.Bifunctor
import Data.Bool (bool)
import Data.Either (partitionEithers)
import Data.Functor ((<&>))
import Data.List (find, foldl', isPrefixOf, stripPrefix, transpose)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust, isNothing, listToMaybe)

type Input = [Vent]

data Vent = Vent (Word, Word) (Word, Word)
  deriving (Show)

range :: (Ord a, Enum a) => a -> a -> [a]
range a b = [min a b .. max a b]

splitOn :: Eq a => [a] -> [a] -> ([a], [a])
splitOn delimiter = go id
  where
    go done [] = (done [], [])
    go done all@(x : rest) = case stripPrefix delimiter all of
      Just s -> (done [], s)
      Nothing -> go ((x :) <&> done) rest

parseVent :: [String] -> [Vent]
parseVent = fmap (go . splitOn " -> ")
  where
    go (first, last) =
      let parse = bimap read read . splitOn ","
       in Vent (parse first) (parse last)

readInput :: FilePath -> IO Input
readInput path = parseVent . lines <$> readFile path

run f = readInput "./data/advent5.txt" >>= print . f

test f g = readInput "./data/advent5_test.txt" <&> g . f

overlap :: (Ord k, Num a) => (a -> Bool) -> [k] -> Int
overlap f =
  length
    . Map.elems
    . Map.filter f
    . Map.fromListWith (+)
    . fmap (,1)

{-
>>>  test solve1 (==5)
True

-}

solve1 :: Input -> Int
solve1 = overlap (> 1) . concatMap coords
  where
    coords (Vent (x1, y1) (x2, y2))
      | x1 == x2 = (x1,) <$> range y1 y2
      | y1 == y2 = (,y1) <$> range x1 x2
      | otherwise = []

run1 :: IO ()
run1 = run solve1

{-
>>>  test solve2 (==12)
True

-}

solve2 :: Input -> Int
solve2 = overlap (> 1) . concatMap coords
  where
    coords (Vent (x1, y1) (x2, y2))
      | x1 == x2 = (x1,) <$> range y1 y2
      | y1 == y2 = (,y1) <$> range x1 x2
      | otherwise =
        let xs = (bool reverse id (x1 > x2) $ range x1 x2)
            ys = (bool reverse id (y1 > y2) $ range y1 y2)
         in zip xs ys

run2 :: IO ()
run2 = run solve2
