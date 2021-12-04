module Advent4 where

import Data.Bool (bool)
import Data.Either (partitionEithers)
import Data.List (find, foldl', transpose)
import Data.Maybe (catMaybes, isNothing, listToMaybe)

type Board = [[Maybe Word]]

data Bingo = Bingo [Word] [Board]
  deriving (Show)

chunk :: Int -> [a] -> [[a]]
chunk i [] = []
chunk i list = let (c, rest) = splitAt i list in c : chunk i rest

parseBingo :: [String] -> Bingo
parseBingo (numbers : _ : rest) = Bingo (read ("[" <> numbers <> "]")) boards
  where
    boards = chunk 5 [fmap (Just . read) . words $ l | l <- rest, l /= ""]
parseBingo _ = error "Unexpected"

readData :: IO Bingo
readData = parseBingo . lines <$> readFile "./data/advent4.txt"

run f = readData >>= print . f

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
>>>  solve1 (parseBingo [ "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1", "", "22 13 17 11  0","8  2 23  4 24","21  9 14 16  7","6 10  3 18  5","1 12 20 15 19","","3 15  0  2 22","9 18 13 17  5","19  8  7 25 23","20 11 10 24  4","14 21 16 12  6","","14 21 17 24  4","10 16 15  9 19","18  8 23 26 20","22 11 13  6  5","2  0 12  3  7"]) == 4512
True

-}

solve1 :: Bingo -> Word
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
>>>  solve2 (parseBingo [ "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1", "", "22 13 17 11  0","8  2 23  4 24","21  9 14 16  7","6 10  3 18  5","1 12 20 15 19","","3 15  0  2 22","9 18 13 17  5","19  8  7 25 23","20 11 10 24  4","14 21 16 12  6","","14 21 17 24  4","10 16 15  9 19","18  8 23 26 20","22 11 13  6  5","2  0 12  3  7"]) == 1924
True

-}

solve2 :: Bingo -> Word
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
