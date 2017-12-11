-- Solution to Day 11 of the Advent Of Code 2017

module Main where

import Data.List (foldl', scanl')

main :: IO ()
main = do
  inp <- readInput
  putStrLn $ "part 1: " ++ show (part1 inp)
  putStrLn $ "part 2: " ++ show (part2 inp)


type Input = [Move]


data Move
  = North
  | NorthEast
  | SouthEast
  | South
  | SouthWest
  | NorthWest
  deriving Show


type Coord = (Int,Int)


part1 :: [Move] -> Int
part1 = hexDist start . moves start


part2 :: [Move] -> Int
part2 = maximum . map (hexDist start) . coords start


start :: Coord
start = (0,0)

move :: Move -> Coord -> Coord
move North (x,y)     = (x,y+1)
move NorthEast (x,y) = (x+1,y+1)
move SouthEast (x,y) = (x+1,y)
move South (x,y)     = (x,y-1)
move SouthWest (x,y) = (x-1,y-1)
move NorthWest (x,y) = (x-1,y)


moves :: Coord -> [Move] -> Coord
moves start = foldl' (flip move) start


coords :: Coord -> [Move] -> [Coord]
coords start = scanl' (flip move) start


hexDist :: Coord -> Coord -> Int
hexDist (x,y) (x',y') =
  let dX = x - x'
      dY = y -y'
      dD = dY - dX
  in maximum $ map abs [dX,dY,dD]


instance Read Move where
  readsPrec _ ('n':'e':rest) = [(NorthEast, rest)]
  readsPrec _ ('n':'w':rest) = [(NorthWest, rest)]
  readsPrec _ ('n':rest)     = [(North, rest)]
  readsPrec _ ('s':'e':rest) = [(SouthEast, rest)]
  readsPrec _ ('s':'w':rest) = [(SouthWest, rest)]
  readsPrec _ ('s':rest)     = [(South, rest)]
  readsPrec _ _              = []


readInput :: IO Input
readInput = readMoves <$> readFile "input.txt"


readMoves :: String -> [Move]
readMoves inp = read ('[' : inp ++ "]")
