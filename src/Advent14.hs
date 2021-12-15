{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Advent14 where

import Control.Arrow ((***))
import Data.Bifunctor (bimap, first)
import Data.Functor ((<&>))
import Data.List (foldl', group, isSubsequenceOf, scanl', sort, stripPrefix)
import qualified Data.Map.Lazy as Map
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as Set

type Input = Polymer

data Polymer = Polymer (Map.Map (Char, Char) Int) (Map.Map (Char, Char) Char)

splitOn :: Eq a => [a] -> [a] -> ([a], [a])
splitOn delimiter = go id
  where
    go done [] = (done [], [])
    go done all@(x : rest) = case stripPrefix delimiter all of
      Just s -> (done [], s)
      Nothing -> go ((x :) <&> done) rest

parsePolymer :: String -> Polymer
parsePolymer = uncurry Polymer . (parseInitial *** parseRules) . splitOn "\n\n"
  where
    parseRules = Map.fromList . fmap parseRule . lines
    parseRule s = let ([a, b], [to]) = splitOn " -> " s in ((a, b), to)

    parseInitial = Map.fromListWith (+) . fmap (,1) . parsePair
    parsePair s = zip s (tail s <> ".")

readInput :: FilePath -> IO Input
readInput path = parsePolymer <$> readFile path

run f = readInput "./data/advent14.txt" >>= print . f

test f g = readInput "./data/advent14_test.txt" <&> g . f

count :: Map.Map (Char, b) Int -> Int
count = ((-) <$> maximum <*> minimum) . Map.elems . Map.mapKeysWith (+) fst

step :: Map.Map (Char, Char) Char -> Map.Map (Char, Char) Int -> Map.Map (Char, Char) Int
step rules pairs = foldl' (apply rules) pairs (Map.assocs pairs)

apply :: Map.Map (Char, Char) Char -> Map.Map (Char, Char) Int -> ((Char, Char), Int) -> Map.Map (Char, Char) Int
apply rules acc ((a, b), n) = case Map.lookup (a, b) rules of
  Just c -> Map.unionWith (+) acc $ Map.fromListWith (+) [((a, b), - n), ((a, c), n), ((c, b), n)]
  Nothing -> acc

{-
>>>  test solve1 (==1588)
True
-}

solve1 :: Input -> Int
solve1 (Polymer start rules) = count (steps !! 10)
  where
    steps = iterate (step rules) start

run1 :: IO ()
run1 = run solve1

{-
>>>  test solve2 (==2188189693529)
True

-}
solve2 :: Input -> Int
solve2 (Polymer start rules) = count (steps !! 40)
  where
    steps = iterate (step rules) start

run2 :: IO ()
run2 = run solve2
