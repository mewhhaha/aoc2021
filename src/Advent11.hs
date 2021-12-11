{-# LANGUAGE ViewPatterns #-}

module Advent11 where

import Control.Monad ((>=>))
import Data.Char (digitToInt)
import Data.Functor ((<&>))
import Data.List (foldl')
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, isNothing, mapMaybe)

type Input = Map.Map (Int, Int) Int

readMap :: String -> Map.Map (Int, Int) Int
readMap = Map.fromList . go 0 0
  where
    go _ _ [] = []
    go _ y ('\n' : ss) = go 0 (y + 1) ss
    go x y (s : ss) = ((x, y), digitToInt s) : go (x + 1) y ss

readInput :: FilePath -> IO Input
readInput path = readMap <$> readFile path

run f = readInput "./data/advent11.txt" >>= print . f

test f g = readInput "./data/advent11_test.txt" <&> g . f

adjacent (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1), (x + 1, y + 1), (x - 1, y - 1), (x + 1, y - 1), (x - 1, y + 1)]

flash v = if v > 9 then 0 else v

step :: Map.Map (Int, Int) Int -> Map.Map (Int, Int) Int
step (Map.map (flash . succ) -> m) = go (Map.keys . Map.filter (== 0) $ m) m
  where
    go :: [(Int, Int)] -> Map.Map (Int, Int) Int -> Map.Map (Int, Int) Int
    go [] area = area
    go (f : fs) area =
      let unflashed = filter ((`Map.lookup` area) <&> maybe False (/= 0)) $ adjacent f
          flashed = foldl' (flip (Map.update (pure . flash . succ))) area unflashed
          reaction = filter ((`Map.lookup` flashed) <&> (== Just 0)) unflashed
       in go (reaction ++ fs) flashed

{-
>>>  test solve1 id
1656
-}

solve1 :: Input -> Int
solve1 = go 0 0
  where
    go acc 100 _ = acc
    go acc n m =
      let next = step m
          flashes = Map.size $ Map.filter (== 0) next
       in go (acc + flashes) (succ n) next

run1 :: IO ()
run1 = run solve1

{-
>>>  test solve2 id
195

-}

solve2 :: Input -> Int
solve2 m = go 1 m
  where
    everyone = Map.size m
    go n m =
      let next = step m
          flashes = Map.size $ Map.filter (== 0) next
       in if flashes == everyone then n else go (succ n) next

run2 :: IO ()
run2 = run solve2
