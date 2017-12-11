-- Solution to Day 11 of the Advent Of Code 2017

module Main where

import Data.List (foldl', scanl')
import Data.Monoid ((<>), Sum(..))


main :: IO ()
main = do
  inp <- readInput
  putStrLn $ "part 1: " ++ show (part1 inp)
  putStrLn $ "part 2: " ++ show (part2 inp)


----------------------------------------------------------------------
-- data and types

type Input = [Move]


data Move
  = North
  | NorthEast
  | SouthEast
  | South
  | SouthWest
  | NorthWest
  deriving Show


type Coord = (Sum Int, Sum Int)


----------------------------------------------------------------------
-- solutions

part1 :: [Move] -> Int
part1 = hexDist . moves

part2 :: [Move] -> Int
part2 = maximum . map hexDist . coords


----------------------------------------------------------------------
-- hex-coord helpers

direction :: Move -> Coord
direction North     = (0,  1)
direction NorthEast = (1,  1)
direction SouthEast = (1,  0)
direction South     = (0, -1)
direction SouthWest = (-1,-1)
direction NorthWest = (-1, 0)


moves :: [Move] -> Coord
moves = foldMap direction


coords :: [Move] -> [Coord]
coords = scanl' (\coord move -> coord <> direction move) mempty


hexDist :: Coord -> Int
hexDist (Sum x, Sum y) =
  maximum $ map abs [x, y, x-y]


----------------------------------------------------------------------
-- read input

readInput :: IO Input
readInput = readMoves <$> readFile "input.txt"


readMoves :: String -> [Move]
readMoves inp = read ('[' : inp ++ "]")


instance Read Move where
  readsPrec _ ('n':'e':rest) = [(NorthEast, rest)]
  readsPrec _ ('n':'w':rest) = [(NorthWest, rest)]
  readsPrec _ ('n':rest)     = [(North, rest)]
  readsPrec _ ('s':'e':rest) = [(SouthEast, rest)]
  readsPrec _ ('s':'w':rest) = [(SouthWest, rest)]
  readsPrec _ ('s':rest)     = [(South, rest)]
  readsPrec _ _              = []
