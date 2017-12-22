-- Solution to Day 22 of the Advent Of Code 2017

module Main where

import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  grd <- readInput

  let part2 = infected $ doBursts 10000000 grd
  putStrLn $ "part 2: " ++ show part2


type Input = Grid

data Grid =
  Grid
  { stateCoords    :: M.Map Coord State
  , carrierAt      :: Coord
  , carrierDir     :: Direction
  , infected       :: Int
  } deriving Show

type Coord = (Int,Int)

data Direction = DUp | DLeft | DRight | DDown
  deriving Show


data State = Clean | Weakened | Infected | Flagged
  deriving Show

doBursts :: Int -> Grid -> Grid
doBursts n = nTimes n burst


burst :: Grid -> Grid
burst = move . toggleInfection . turn


turn :: Grid -> Grid
turn grd =
  case getState grd of
    Clean    -> grd { carrierDir = rotateLeft (carrierDir grd) }
    Weakened -> grd
    Infected -> grd { carrierDir = rotateRight (carrierDir grd) }
    Flagged  -> grd { carrierDir = reverseDir (carrierDir grd) }


toggleInfection :: Grid -> Grid
toggleInfection grd =
  case getState grd of
    Clean    -> setState Weakened grd
    Weakened -> countInfection $ setState Infected grd
    Infected -> setState Flagged grd
    Flagged  -> resetState grd


move :: Grid -> Grid
move grd = grd { carrierAt = moveIn (carrierDir grd) (carrierAt grd) }


countInfection :: Grid -> Grid
countInfection grd = grd { infected = infected grd + 1 }


getState :: Grid -> State
getState grd = fromMaybe Clean $ carrierAt grd `M.lookup` stateCoords grd


setState :: State -> Grid -> Grid
setState state grd = grd { stateCoords = M.insert (carrierAt grd) state (stateCoords grd) }


resetState :: Grid -> Grid
resetState grd = grd { stateCoords = M.delete (carrierAt grd) (stateCoords grd) }


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


reverseDir :: Direction -> Direction
reverseDir DUp    = DDown
reverseDir DRight = DLeft
reverseDir DDown  = DUp
reverseDir DLeft  = DRight


moveIn :: Direction -> Coord -> Coord
moveIn DUp    (x,y) = (x,   y-1)
moveIn DDown  (x,y) = (x,   y+1)
moveIn DLeft  (x,y) = (x-1, y)
moveIn DRight (x,y) = (x+1, y)


nTimes :: Int -> (a -> a) -> a -> a
nTimes 0 _ x = x
nTimes n f x = nTimes (n-1) f (f x)


readInput :: IO Input
readInput = parseInput . lines <$> readFile "../input.txt"


parseInput :: [String] -> Grid
parseInput ls =
  let infGrd =
        M.fromList
        $ map (\ ((x,y), inf) -> ((x,y), Infected))
        $ filter snd
        $ concatMap (\(y,cs) -> zipWith (\x c -> ((x,y), c == '#')) [0..] cs)
        $ zip [0..] ls
      my = length ls `div` 2
      mx = length (head ls) `div` 2
  in Grid infGrd (mx,my) DUp 0
