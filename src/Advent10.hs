module Advent10 where

import Data.Either (lefts, rights)
import Data.Functor ((<&>))
import Data.List (foldl', sort)

data Bracket = Curve | Square | Curly | Pointy
  deriving (Eq)

data Token = Opening Bracket | Closing Bracket
  deriving (Eq)

type Input = [[Token]]

parseTokens :: String -> [Token]
parseTokens = fmap parseToken
  where
    parseToken '(' = Opening Curve
    parseToken '[' = Opening Square
    parseToken '{' = Opening Curly
    parseToken '<' = Opening Pointy
    parseToken ')' = Closing Curve
    parseToken ']' = Closing Square
    parseToken '}' = Closing Curly
    parseToken '>' = Closing Pointy
    parseToken x = error ("Unexpected character: " <> show x)

readInput :: FilePath -> IO Input
readInput path = fmap parseTokens . lines <$> readFile path

run f = readInput "./data/advent10.txt" >>= print . f

test f g = readInput "./data/advent10_test.txt" <&> g . f

-- Left is corrupted, right is unfinished
validate :: [Token] -> Either Bracket [Bracket]
validate = go []
  where
    go :: [Bracket] -> [Token] -> Either Bracket [Bracket]
    go stack ((Opening x) : xs) = go (x : stack) xs
    go (y : ys) ((Closing x) : xs)
      | y == x = go ys xs
      | otherwise = Left x
    go stack [] = Right stack
    go [] _ = error "Unexpected valid syntax"

{-
>>>  test solve1 id
26397
-}

solve1 :: Input -> Int
solve1 = sum . fmap score . lefts . fmap validate
  where
    score Curve = 3
    score Square = 57
    score Curly = 1197
    score Pointy = 25137

run1 :: IO ()
run1 = run solve1

{-
>>>  test solve2 id
288957

-}

solve2 :: Input -> Int
solve2 = middle . sort . fmap score . rights . fmap validate
  where
    middle xs = xs !! (length xs `div` 2)

    score xs = foldl' (\acc x -> acc * 5 + points x) 0 xs
      where
        points Curve = 1
        points Square = 2
        points Curly = 3
        points Pointy = 4

run2 :: IO ()
run2 = run solve2