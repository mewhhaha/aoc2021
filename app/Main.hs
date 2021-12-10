module Main where

import qualified Advent1
import qualified Advent10
import qualified Advent2
import qualified Advent3
import qualified Advent4
import qualified Advent5
import qualified Advent6
import qualified Advent7
import qualified Advent8
import qualified Advent9
import Control.Monad (zipWithM_)

main :: IO ()
main = do
  zipWithM_
    ( \day (part1, part2) -> do
        let title part = putStrLn $ "Advent day " <> show day <> " part " <> show part
        title "1"
        part1
        title "2"
        part2
    )
    [1 ..]
    [ (Advent1.run1, Advent1.run2),
      (Advent2.run1, Advent2.run2),
      (Advent3.run1, Advent3.run2),
      (Advent4.run1, Advent4.run2),
      (Advent5.run1, Advent5.run2),
      (Advent6.run1, Advent6.run2),
      (Advent7.run1, Advent7.run2),
      (Advent8.run1, Advent8.run2),
      (Advent9.run1, Advent9.run2),
      (Advent10.run1, Advent10.run2)
    ]