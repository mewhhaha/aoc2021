module Advent12 where

import Control.Monad.State
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

isSmall :: String -> Bool
isSmall = all isLower

{-
>>> test solve1 id
226
-}

solve1 :: Input -> Int
solve1 input = go input "start"
  where
    go :: Map.Map String [String] -> String -> Int
    go _ "end" = 1
    go nodes node =
      let continue vs = sum $ fmap (go vs) (nodes Map.! node)
       in if isSmall node then continue (Map.insert node [] nodes) else continue nodes

run1 :: IO ()
run1 = run solve1

data ExtraVisit = ExtraUsed | ExtraUnused
  deriving (Eq, Ord, Show)

{-
>>>  test solve2 id
3509

-}

solve2 :: Input -> Int
solve2 input = Set.size . Set.fromList $ go ExtraUnused input "start"
  where
    go :: ExtraVisit -> Map.Map String [String] -> String -> [[String]]
    go _ _ "end" = [["end"]]
    go extra nodes node =
      let continue e vs = concatMap (fmap (node :) . go e vs) (nodes Map.! node)
       in case (isSmall node, extra) of
            (True, ExtraUsed) -> continue ExtraUsed (Map.insert node [] nodes)
            (True, ExtraUnused) -> continue ExtraUnused (Map.insert node [] nodes) <> continue ExtraUsed nodes
            _ -> continue extra nodes

run2 :: IO ()
run2 = run solve2
