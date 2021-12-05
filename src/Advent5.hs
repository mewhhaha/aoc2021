{-# LANGUAGE TupleSections #-}

module Advent5 where

import Data.Bifunctor
import Data.Bool (bool)
import Data.Either (partitionEithers)
import Data.List (find, foldl', isPrefixOf, stripPrefix, transpose)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust, isNothing, listToMaybe)

splitOn :: Eq a => [a] -> [a] -> ([a], [a])
splitOn delimiter = go . ([],)
  where
    go (done, []) = (reverse done, [])
    go (done, rest) | delimiter `isPrefixOf` rest = (reverse done, fromJust . stripPrefix delimiter $ rest)
    go (done, x : rest) = go (x : done, rest)

data Vent = Vent (Word, Word) (Word, Word)
  deriving (Show)

parseVent :: [String] -> [Vent]
parseVent = fmap (go . splitOn " -> ")
  where
    go (first, last) =
      let parse = bimap read read . splitOn ","
       in Vent (parse first) (parse last)

readData :: IO [Vent]
readData = parseVent . lines <$> readFile "./data/advent5.txt"

run f = readData >>= print . f

overlap f =
  length
    . Map.elems
    . Map.filter f
    . Map.fromListWith (+)
    . fmap (,1)

{-
>>>  5 == solve1 (parseVent ["0,9 -> 5,9","8,0 -> 0,8","9,4 -> 3,4","2,2 -> 2,1","7,0 -> 7,4","6,4 -> 2,0","0,9 -> 2,9","3,4 -> 1,4","0,0 -> 8,8","5,5 -> 8,2"])
True

-}

solve1 :: [Vent] -> Int
solve1 = overlap (> 1) . concatMap coords
  where
    range a b = [min a b .. max a b]
    coords (Vent (x1, y1) (x2, y2))
      | x1 == x2 = (x1,) <$> range y1 y2
      | y1 == y2 = (,y1) <$> range x1 x2
      | otherwise = []

run1 :: IO ()
run1 = run solve1

{-
>>>  12 == solve2 (parseVent ["0,9 -> 5,9","8,0 -> 0,8","9,4 -> 3,4","2,2 -> 2,1","7,0 -> 7,4","6,4 -> 2,0","0,9 -> 2,9","3,4 -> 1,4","0,0 -> 8,8","5,5 -> 8,2"])
True

-}

solve2 :: [Vent] -> Int
solve2 = overlap (> 1) . concatMap coords
  where
    range a b = [min a b .. max a b]
    coords (Vent (x1, y1) (x2, y2))
      | x1 == x2 = (x1,) <$> range y1 y2
      | y1 == y2 = (,y1) <$> range x1 x2
      | y1 > y2 && x1 > x2 = zip [x1, x1 - 1 .. x2] [y1, y1 - 1 .. y2]
      | y1 < y2 && x1 > x2 = zip [x1, x1 - 1 .. x2] [y1 .. y2]
      | y1 > y2 && x1 < x2 = zip [x1 .. x2] [y1, y1 - 1 .. y2]
      | y1 < y2 && x1 < x2 = zip [x1 .. x2] [y1 .. y2]
      | otherwise = error "Unexpected"

run2 :: IO ()
run2 = run solve2
