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

offspring :: Int -> Int -> State (Map.Map Int Int) Int
offspring t days
  | days <= 0 = return 0
  | otherwise =
    let days' = days - t
     in gets (Map.lookup days') >>= \case
          Just value -> return value
          Nothing
            | t == 0 -> do
              let tomorrow = days - 1
              v1 <- offspring 6 tomorrow
              v2 <- (1 +) <$> offspring 8 tomorrow
              return (v1 + v2)
            | otherwise -> do
              let days' = days - t
              v <- offspring 0 days'
              modify (Map.insert days' v)
              return v

{-
>>>  solve1 (parseFish ["3","4","3","1","2"])
5934

-}

solve1 :: Input -> Int
solve1 = flip evalState mempty . foldM go 0
  where
    go v (t, n) = do
      v' <- offspring t 80
      return (n + n * v' + v)

run1 :: IO ()
run1 = run solve1

{-
>>>  solve2 (parseFish ["3","4","3","1","2"])
26984457539

-}

solve2 :: Input -> Int
solve2 = flip evalState mempty . foldM go 0
  where
    go v (t, n) = do
      v' <- offspring t 256
      return (n + n * v' + v)

run2 :: IO ()
run2 = run solve2
