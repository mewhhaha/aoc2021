module Advent0 where

import Data.Functor ((<&>))

type Input = [String]

readInput :: FilePath -> IO Input
readInput path = undefined <$> readFile path

run f = readInput "./data/advent0.txt" >>= print . f

test f g = readInput "./data/advent0_test.txt" <&> g . f

{-
>>>  test solve1 id
-}

solve1 :: Input -> Int
solve1 = undefined

run1 :: IO ()
run1 = run solve1

{-
>>>  test solve2 id

-}

solve2 :: Input -> Int
solve2 = undefined

run2 :: IO ()
run2 = run solve2
