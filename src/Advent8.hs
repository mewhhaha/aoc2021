{-# LANGUAGE TupleSections #-}

module Advent8 where

import Data.Functor ((<&>))
import Data.List (find, groupBy, isPrefixOf, sort, stripPrefix)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import Data.Tuple (swap)

data Display = Display [String] [String]

type Input = [Display]

splitOn :: Eq a => [a] -> [a] -> ([a], [a])
splitOn delimiter = go . ([],)
  where
    go (done, []) = (reverse done, [])
    go (done, rest) | delimiter `isPrefixOf` rest = (reverse done, fromJust . stripPrefix delimiter $ rest)
    go (done, x : rest) = go (x : done, rest)

parseDisplay :: (String, String) -> Display
parseDisplay (input, output) = Display (words input) (words output)

readInput :: FilePath -> IO Input
readInput path = fmap (parseDisplay . splitOn " | ") . lines <$> readFile path

run f = readInput "./data/advent8.txt" >>= print . f

test f g = readInput "./data/advent8_test.txt" <&> g . f

zero, one, two, three, four, five, six, seven, eight, nine :: [Char]
zero = "abcefg"
two = "acdeg"
three = "acdfg"
five = "abdfg"
six = "abdefg"
nine = "abcdfg"
-- Unique lengths
one = "cf" -- cf wiring
four = "bcdf" -- bd wiring
seven = "acf" -- a wiring
eight = "abcdefg" -- eg

{-
>>>  test solve1 id
26

-}

solve1 :: Input -> Int
solve1 = sum . fmap go
  where
    uniqueLengths = [length one, length four, length seven, length eight]
    go (Display _ output) = length . filter (\x -> let len = length x in elem len uniqueLengths) $ output

run1 :: IO ()
run1 = run solve1

type Wiring = Map.Map Char Char

{-
>>>  test solve2 id
Just 61229

-}

solve2 :: Input -> Maybe Int
solve2 = fmap sum . mapM go
  where
    numbers = [zero, one, two, three, four, five, six, seven, eight, nine]

    go (Display input output) = do
      let findNumber x = find ((== length x) . length)

      cf <- findNumber one input
      bcdf <- findNumber four input
      acf <- findNumber seven input
      abcdefg <- findNumber eight input

      let abcdf = [a, mb, mc, md, mf]
          [a] = filter (`notElem` cf) acf
          [mc, mf] = cf
          [mb, md] = filter (`notElem` cf) bcdf
          [me, mg] = filter (`notElem` abcdf) abcdefg

          wirings =
            [ Map.fromList (zip [a, b, c, d, e, f, g] ['a' ..])
              | (b, d) <- [(mb, md), (md, mb)],
                (e, g) <- [(me, mg), (mg, me)],
                (c, f) <- [(mc, mf), (mf, mc)]
            ]

          validate w =
            let rewired = sort . mapMaybe (`Map.lookup` w) <$> input
             in all (`elem` numbers) rewired

      wiring <- find validate wirings

      let decode x = lookup x $ zip numbers ['0' ..]
          rewire = fmap sort . mapM (`Map.lookup` wiring)
          digit x = rewire x >>= decode

      read <$> mapM digit output

run2 :: IO ()
run2 = run solve2
