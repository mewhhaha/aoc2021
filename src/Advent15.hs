{-# LANGUAGE TupleSections #-}

module Advent15 where

import Control.Monad
import Data.Bifunctor (bimap)
import Data.Functor ((<&>))
import Data.List (foldl', uncons)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Set as Set
import Data.Tuple (swap)
import Debug.Trace (trace)

type Input = Map.Map (Int, Int) Int

parseMap :: String -> Map.Map (Int, Int) Int
parseMap = go 0 0
  where
    go x y [] = mempty
    go x y ('\n' : cs) = go 0 (y + 1) cs
    go x y (c : cs) = Map.insert (x, y) (read [c]) (go (x + 1) y cs)

readInput :: FilePath -> IO Input
readInput path = parseMap <$> readFile path

run f = readInput "./data/advent15.txt" >>= print . f

test f g = readInput "./data/advent15_test.txt" <&> g . f

adjacent (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

findLowestRisk :: Map.Map (Int, Int) Int -> (Int, Int) -> (Int, Int) -> Maybe (Int, Map.Map (Int, Int) Int)
findLowestRisk nodes start end = go (Set.singleton (0, start)) (Map.singleton start 0)
  where
    go :: Set.Set (Int, (Int, Int)) -> Map.Map (Int, Int) Int -> Maybe (Int, Map.Map (Int, Int) Int)
    go todo scores = do
      ((value, current), open) <- Set.minView todo

      let neighbours = mapMaybe (\key -> (key,) <$> Map.lookup key nodes) (adjacent current)

          check (neighbour, cost) =
            if currentScore < neighbourScore
              then Just (neighbour, currentScore)
              else Nothing
            where
              currentScore = Map.findWithDefault maxBound current scores + cost
              neighbourScore = Map.findWithDefault maxBound neighbour scores

          paths = mapMaybe check neighbours

          openNext = foldl' (flip Set.insert) open (swap <$> paths)
          scoresNext = foldl' (flip . uncurry $ Map.insert) scores paths

      if current == end
        then Just (value, scores)
        else go openNext scoresNext

{-
>>>  test solve1 id
Just 40
-}

solve1 :: Input -> Maybe Int
solve1 nodes = fst <$> findLowestRisk nodes (0, 0) lowerRight
  where
    (Just ((lowerRight, _), _)) = Map.maxViewWithKey nodes

run1 :: IO ()
run1 = run solve1

scaleUpBy :: Int -> Map.Map (Int, Int) Int -> Map.Map (Int, Int) Int
scaleUpBy n nodes = Map.unions maps
  where
    maps = do
      let (((width, height), _), _) = fromJust . Map.maxViewWithKey $ nodes
      offsetX <- [0 .. (n - 1)]
      offsetY <- [0 .. (n - 1)]

      let offsetKey = bimap (+ ((1 + width) * offsetX)) (+ ((1 + height) * offsetY))
          offsetValue v = (v + (offsetX + offsetY) - 1) `mod` 9 + 1
      return . Map.map offsetValue . Map.mapKeys offsetKey $ nodes

{-
>>>  test solve2 id
Just 315

-}

solve2 :: Input -> Maybe Int
solve2 nodes = do
  (value, values) <- findLowestRisk scaledNodes (0, 0) lowerRight

  pure value
  where
    scaledNodes = scaleUpBy 5 nodes
    (Just ((lowerRight, _), _)) = Map.maxViewWithKey scaledNodes

run2 :: IO ()
run2 = run solve2
