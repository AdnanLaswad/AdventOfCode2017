-- Solution to Day 22 of the Advent Of Code 2017

module Main where

import qualified Data.Set as S

main :: IO ()
main = do
  grd <- readInput

  let part1 = infected $ doBursts 10000 grd
  putStrLn $ "part 1: " ++ show part1


type Input = Grid

data Grid =
  Grid
  { infectedCoords :: S.Set Coord
  , carrierAt      :: Coord
  , carrierDir     :: Direction
  , infected       :: Int
  } deriving Show

type Coord = (Int,Int)

data Direction = DUp | DLeft | DRight | DDown
  deriving Show


doBursts :: Int -> Grid -> Grid
doBursts n = nTimes n burst


burst :: Grid -> Grid
burst = move . toggleInfection . turn


turn :: Grid -> Grid
turn grd =
  if isInfected grd
  then grd { carrierDir = rotateRight (carrierDir grd) }
  else grd { carrierDir = rotateLeft (carrierDir grd) }


toggleInfection :: Grid -> Grid
toggleInfection grd =
  if isInfected grd
  then grd { infectedCoords = carrierAt grd `S.delete` infectedCoords grd }
  else grd { infectedCoords = carrierAt grd `S.insert` infectedCoords grd
           , infected       = infected grd + 1
           }


move :: Grid -> Grid
move grd = grd { carrierAt = moveIn (carrierDir grd) (carrierAt grd) }


isInfected :: Grid -> Bool
isInfected grd = carrierAt grd `S.member` infectedCoords grd


rotateLeft :: Direction -> Direction
rotateLeft DUp    = DLeft
rotateLeft DLeft  = DDown
rotateLeft DDown  = DRight
rotateLeft DRight = DUp


rotateRight :: Direction -> Direction
rotateRight DUp    = DRight
rotateRight DRight = DDown
rotateRight DDown  = DLeft
rotateRight DLeft  = DUp


moveIn :: Direction -> Coord -> Coord
moveIn DUp    (x,y) = (x,   y-1)
moveIn DDown  (x,y) = (x,   y+1)
moveIn DLeft  (x,y) = (x-1, y)
moveIn DRight (x,y) = (x+1, y)


nTimes :: Int -> (a -> a) -> a -> a
nTimes 0 _ x = x
nTimes n f x = nTimes (n-1) f (f x)


readInput :: IO Input
readInput = parseInput . lines <$> readFile "input.txt"


parseInput :: [String] -> Grid
parseInput ls =
  let infGrd =
        S.fromList
        $ map fst
        $ filter snd
        $ concatMap (\(y,cs) -> zipWith (\x c -> ((x,y), c == '#')) [0..] cs)
        $ zip [0..] ls
      my = length ls `div` 2
      mx = length (head ls) `div` 2
  in Grid infGrd (mx,my) DUp 0
