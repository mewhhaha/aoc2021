module Advent4 where

import Data.Bool (bool)
import Data.Either (partitionEithers)
import Data.Functor ((<&>))
import Data.List (find, foldl', transpose)
import Data.Maybe (catMaybes, isNothing, listToMaybe)

type Board = [[Maybe Word]]

data Bingo = Bingo [Word] [Board]
  deriving (Show)

type Input = Bingo

chunk :: Int -> [a] -> [[a]]
chunk i [] = []
chunk i list = let (c, rest) = splitAt i list in c : chunk i rest

parseBingo :: [String] -> Bingo
parseBingo (numbers : _ : rest) = Bingo (read ("[" <> numbers <> "]")) boards
  where
    boards = chunk 5 [fmap (Just . read) . words $ l | l <- rest, l /= ""]
parseBingo _ = error "Unexpected"

readInput :: FilePath -> IO Input
readInput path = parseBingo . lines <$> readFile path

run f = readInput "./data/advent4.txt" >>= print . f

test f g = readInput "./data/advent4_test.txt" <&> g . f

validate :: Board -> Bool
validate board = horizontal || vertical
  where
    winner = all isNothing
    horizontal = or (winner <$> board)
    vertical = or (winner <$> transpose board)

update :: Word -> Board -> Board
update n = (fmap . fmap) (\x -> if x == Just n then Nothing else x)

score :: Board -> Word
score = sum . catMaybes . concat

{-
>>> test solve1 (==4512)
True

-}

solve1 :: Input -> Word
solve1 = go
  where
    go (Bingo (n : rest) boards) =
      let next = update n <$> boards
          winner = find validate next
       in case winner of
            Just board -> n * score board
            _ -> go (Bingo rest next)
    go _ = error "Unexpected"

run1 :: IO ()
run1 = run solve1

{-
>>> test solve2 (==1924)
True

-}

solve2 :: Input -> Word
solve2 = go
  where
    go (Bingo (n : rest) boards) =
      let next = update n <$> boards
          waiting = filter (not . validate) next
          loser = if null waiting then Just (last next) else Nothing
       in case loser of
            Just board -> n * score board
            _ -> go (Bingo rest waiting)
    go _ = error "Unexpected"

run2 :: IO ()
run2 = run solve2
