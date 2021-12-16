module Main where

import qualified Advent1
import qualified Advent10
import qualified Advent11
import qualified Advent12
import qualified Advent13
import qualified Advent14
import qualified Advent15
import qualified Advent16
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
      (Advent10.run1, Advent10.run2),
      (Advent11.run1, Advent11.run2),
      (Advent12.run1, Advent12.run2),
      (Advent13.run1, Advent13.run2),
      (Advent14.run1, Advent14.run2),
      (Advent15.run1, Advent15.run2),
      (Advent16.run1, Advent16.run2)
    ]