module Main where

import qualified Advent1
import qualified Advent2
import qualified Advent3

main :: IO ()
main = do
  putStrLn "Advent 1 Part 1"
  Advent1.run1

  putStrLn "Advent 1 Part 2"
  Advent1.run2

  putStrLn "Advent 2 Part 1"
  Advent2.run1

  putStrLn "Advent 2 Part 2"
  Advent2.run2

  putStrLn "Advent 3 Part 1"
  Advent3.run1

  putStrLn "Advent 3 Part 2"
  Advent3.run2
