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
    go _ y ('\n' : ss) = go 0 (succ y) ss
    go x y (s : ss) = ((x, y), digitToInt s) : go (succ x) y ss

readInput :: FilePath -> IO Input
readInput path = readMap <$> readFile path

run f = readInput "./data/advent11.txt" >>= print . f

test f g = readInput "./data/advent11_test.txt" <&> g . f

adjacent :: (Int, Int) -> [(Int, Int)]
adjacent (x, y) = [(succ x, y), (pred x, y), (x, succ y), (x, pred y), (succ x, succ y), (pred x, pred y), (succ x, pred y), (pred x, succ y)]

flash :: Int -> Int
flash v = if v > 9 then 0 else v

isFlashed :: Int -> Bool
isFlashed = (== 0)

step :: Map.Map (Int, Int) Int -> Map.Map (Int, Int) Int
step = go <$> initialReaction <*> id <$> initialPowerup
  where
    powerup = flash . succ

    initialReaction = Map.keys . Map.filter isFlashed
    initialPowerup = Map.map powerup

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
    count = Map.size . Map.filter isFlashed

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
