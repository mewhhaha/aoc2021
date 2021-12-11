module Advent11 where

import Control.Monad ((>=>))
import Data.Char (digitToInt)
import Data.Functor ((<&>))
import Data.List (find, foldl')
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

adjacent :: (Int, Int) -> [(Int, Int)]
adjacent (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1), (x + 1, y + 1), (x - 1, y - 1), (x + 1, y - 1), (x - 1, y + 1)]

flash :: Int -> Int
flash v = if v > 9 then 0 else v

step :: Map.Map (Int, Int) Int -> Map.Map (Int, Int) Int
step = go <$> initial <*> id <$> energized
  where
    isFlashed = (== 0)

    powerup = flash . succ
    initial = Map.keys . Map.filter isFlashed
    energized = Map.map powerup

    go :: [(Int, Int)] -> Map.Map (Int, Int) Int -> Map.Map (Int, Int) Int
    go [] area = area
    go (f : fs) area =
      let unflashed = filter ((`Map.lookup` area) <&> maybe False (not . isFlashed)) $ adjacent f

          flashed = foldl' (flip (Map.update (pure . powerup))) area unflashed
          reaction = filter ((`Map.lookup` flashed) <&> maybe False isFlashed) unflashed
       in go (reaction ++ fs) flashed

{-
>>>  test solve1 id
1656
-}

solve1 :: Input -> Int
solve1 = sum . fmap count . take 100 . tail . iterate step
  where
    count = Map.size . Map.filter (== 0)

run1 :: IO ()
run1 = run solve1

{-
>>>  test solve2 id
195

-}

solve2 :: Input -> Int
solve2 = length . takeWhile (sum <&> (/= 0)) . iterate step

run2 :: IO ()
run2 = run solve2
