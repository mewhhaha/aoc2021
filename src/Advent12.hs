module Advent12 where

import Control.Monad.State
import Data.Bool
import Data.Char (isLower)
import Data.Functor ((<&>))
import Data.List (delete, foldl', groupBy, sort)
import qualified Data.Map as Map
import qualified Data.Set as Set

split :: Eq a => a -> [a] -> [[a]]
split delimiter = (filter (/= delimiter) <$>) . groupBy (\_ b -> b /= delimiter)

type Input = Map.Map String [String]

readNodes :: [String] -> Map.Map String [String]
readNodes = Map.insert "end" [] . fmap (filter (/= "start")) . foldl1 (Map.unionWith (<>)) . fmap go
  where
    go line = case split '-' line of
      [from, to] -> Map.fromList [(from, [to]), (to, [from])]
      _ -> error ("Unexpected input: " <> show line)

readInput :: FilePath -> IO Input
readInput path = readNodes . lines <$> readFile path

run f = readInput "./data/advent12.txt" >>= print . f

test f g = readInput "./data/advent12_test.txt" <&> g . f

data Size = Small | Big

size :: String -> Size
size node = if all isLower node then Small else Big

{-
>>> test solve1 id
226
-}

solve1 :: Input -> Int
solve1 input = go input "start"
  where
    go :: Map.Map String [String] -> String -> Int
    go _ "end" = 1
    go nodes node = sum (go next <$> (nodes Map.! node))
      where
        next = case size node of
          Small -> Map.insert node [] nodes
          Big -> nodes

run1 :: IO ()
run1 = run solve1

data Visit = CanBacktrack | CannotBacktrack
  deriving (Eq, Ord, Show)

{-
>>>  test solve2 id
3509

-}

solve2 :: Input -> Int
solve2 input = Set.size . Set.fromList $ go input CanBacktrack "start"
  where
    go :: Map.Map String [String] -> Visit -> String -> [[String]]
    go _ _ "end" = [["end"]]
    go nodes backtrack node =
      let continue e vs = concatMap (fmap (node :) . go vs e) (nodes Map.! node)
          next = case size node of
            Small -> Map.insert node [] nodes
            Big -> nodes
       in case (size node, backtrack) of
            (Small, CanBacktrack) -> continue CanBacktrack next <> continue CannotBacktrack nodes
            _ -> continue backtrack next

run2 :: IO ()
run2 = run solve2
