{-# LANGUAGE TupleSections #-}

module Advent13 where

import Control.Monad (forM, forM_)
import Data.Bifunctor (bimap, first, second)
import Data.Functor ((<&>))
import Data.List (foldl', isPrefixOf, scanl', stripPrefix)
import Data.Maybe (fromJust)
import qualified Data.Set as Set

type Input = Paper

data Direction = X | Y

data Fold = Fold Direction Int

data Paper = Paper [(Int, Int)] [Fold]

splitOn :: Eq a => [a] -> [a] -> ([a], [a])
splitOn delimiter = go . ([],)
  where
    go (done, []) = (reverse done, [])
    go (done, rest) | delimiter `isPrefixOf` rest = (reverse done, fromJust . stripPrefix delimiter $ rest)
    go (done, x : rest) = go (x : done, rest)

readPaper :: String -> Paper
readPaper = uncurry Paper . bimap (fmap readPosition . lines) (fmap (readFold . drop (length "fold along ")) . lines) . splitOn "\n\n"
  where
    readFold ('x' : _ : x) = Fold X (read x)
    readFold ('y' : _ : y) = Fold Y (read y)
    readFold xs = error ("Unexpected line: " <> show xs)

    readPosition = bimap read read . splitOn ","

readInput :: FilePath -> IO Input
readInput path = readPaper <$> readFile path

run f = readInput "./data/advent13.txt" >>= print . f

test f g = readInput "./data/advent13_test.txt" <&> g . f

paperFold :: [(Int, Int)] -> Fold -> [(Int, Int)]
paperFold positions (Fold direction i) = squash $ go positions
  where
    squash = Set.toList . Set.fromList
    go = case direction of
      X -> update first
      Y -> update second
    update f = fmap (f (\j -> if j > i then i - (j - i) else j))

{-
>>>  test solve1 (==17)
True
-}

display :: [(Int, Int)] -> IO ()
display positions =
  let ps = Set.fromList positions
      (w, h) = foldl1 (\(x, y) -> bimap (max x) (max y)) positions
   in forM_ [0 .. h] $ \y -> do
        let chars = fmap (\x -> if Set.member (x, y) ps then '#' else '.') [0 .. w]
        putStrLn chars

solve1 :: Input -> Int
solve1 (Paper positions folds) = length $ foldl' paperFold positions (take 1 folds)

run1 :: IO ()
run1 = run solve1

{-
>>>  test solve2 ((==16) . length)
True

-}

solve2 :: Input -> [(Int, Int)]
solve2 (Paper positions folds) = foldl' paperFold positions folds

run2 :: IO ()
run2 = do
  input <- readInput "./data/advent13.txt"
  display (solve2 input)
