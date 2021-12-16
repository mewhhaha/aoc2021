module Advent16 where

import Data.Bifunctor (Bifunctor (bimap, first, second))
import Data.Bool (bool)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.List (foldl', groupBy)
import qualified Data.Map.Strict as Map
import Data.Tuple
import Debug.Trace (trace)

data Value = Literal Int | Operator [Packet]
  deriving (Show)

data Packet = Packet Int Int Value
  deriving (Show)

type Input = [Bool]

parseBits :: String -> [Bool]
parseBits = concatMap (table Map.!)
  where
    table = Map.fromList $ zip (['0' .. '9'] ++ ['A' .. 'F']) [[x, y, z, w] | x <- [False, True], y <- [False, True], z <- [False, True], w <- [False, True]]

readInput :: FilePath -> IO Input
readInput path = parseBits <$> readFile path

run f = readInput "./data/advent16.txt" >>= print . f

test1 f g = readInput "./data/advent16_test1.txt" <&> g . f

test2 f g = readInput "./data/advent16_test2.txt" <&> g . f

chunk :: Int -> [a] -> [[a]]
chunk i [] = []
chunk i list = let (c, rest) = splitAt i list in c : chunk i rest

decimal :: [Bool] -> Int
decimal = foldl' (\acc x -> acc * 2 + if x then 1 else 0) 0

readLiteral :: [Bool] -> ([Bool], Value)
readLiteral bits = Literal . decimal <$> cleanup (parseLiteral $ chunk 5 bits)
  where
    cleanup (rest, literal) = (rest, concat literal)

    parseLiteral ((False : x) : rest) = (concat rest, [x])
    parseLiteral ((True : x) : rest) = (x :) <$> parseLiteral rest
    parseLiteral xs = error ("Unexpected literal: " <> show xs)

readOperator :: [Bool] -> ([Bool], Value)
readOperator (lengthId : rest) =
  if lengthId
    then
      second Operator
        . uncurry operatorNumberOf
        . first decimal
        $ splitAt 11 rest
    else
      second Operator
        . uncurry operatorAbsolute
        . first decimal
        $ splitAt 15 rest
readOperator xs = error ("Unexpected operator: " <> show xs)

operatorAbsolute :: Int -> [Bool] -> ([Bool], [Packet])
operatorAbsolute n bits = swap . first go $ splitAt n bits
  where
    go [] = []
    go bs = let (r, p) = readPacket bs in p : go r

operatorNumberOf :: Int -> [Bool] -> ([Bool], [Packet])
operatorNumberOf 0 bits = (bits, [])
operatorNumberOf n bits = let (r, p) = readPacket bits in (p :) <$> operatorNumberOf (n - 1) r

readPacket :: [Bool] -> ([Bool], Packet)
readPacket bits =
  Packet version typ <$> case typ of
    4 -> readLiteral rest
    _ -> readOperator rest
  where
    (header, rest) = splitAt 6 bits
    (version, typ) = bimap decimal decimal $ splitAt 3 header

sumVersions :: Packet -> Int
sumVersions = go
  where
    go (Packet v _ value) =
      case value of
        Literal _ -> v
        Operator packets -> v + sum (fmap go packets)

{-
>>>  test1 solve1 id
31
-}

solve1 :: Input -> Int
solve1 = sumVersions . snd . readPacket

run1 :: IO ()
run1 = run solve1

eval :: Packet -> Int
eval (Packet _ t (Operator packets)) =
  let op = case t of
        0 -> sum
        1 -> product
        2 -> minimum
        3 -> maximum
        5 -> (\[a, b] -> if a > b then 1 else 0)
        6 -> (\[a, b] -> if a < b then 1 else 0)
        7 -> (\[a, b] -> if a == b then 1 else 0)
        _ -> error ("Unexpected operator type: " <> show t)
   in op (fmap eval packets)
eval (Packet _ 4 (Literal v)) = v
eval packet = error ("Unexpected packet: " <> show packet)

{-
>>>  test2 solve2 id
1

-}

solve2 :: Input -> Int
solve2 = eval . snd . readPacket

run2 :: IO ()
run2 = run solve2
