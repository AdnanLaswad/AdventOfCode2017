-- Solution to Day 13 of the Advent Of Code 2017

module Main where

import Prelude hiding (init, round)

import Data.List (scanl')
import Data.Maybe (mapMaybe, fromMaybe)
import Debug.Trace (trace)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Parser

main :: IO ()
main = do
  inp <- readInput

  putStrLn $ "part 1: " ++ show (part1 inp)


part1 :: Input -> Int
part1 inp =
  let c = init inp
  in sum . map fst $ game c

----------------------------------------------------------------------
-- data and types

type Input = [Scanner]


data Config =
  Config { layout :: Map Layer Scanner
         , step   :: Int
         , pos    :: Int
         } deriving Show


data Scanner =
  Scanner { layer :: Layer
          , range :: Range
          } deriving Show


type Layer = Int

type Range = Int


game :: Config -> [(Int, Config)]
game conf =
  let l = gameSize conf
  in scanl' go (0, conf) (replicate (l+1) ())
  where
    go (_,c) () = round c


init :: Input -> Config
init inp = Config m 0 (-1)
  where m = Map.fromList [ (layer s, s) | s <- inp ]


gameSize :: Config -> Int
gameSize = maximum . map layer . Map.elems . layout


round :: Config -> (Int, Config)
round c =
  let stepIn   = movePos c
      depth    = pos stepIn
      range    = fromMaybe 0 $ getRange c (pos stepIn)
      severity = if isCaught stepIn then depth * range else 0
      nextC    = moveScanner stepIn
  in (severity, nextC)


isCaught :: Config -> Bool
isCaught c =
  let
    p  = pos c
    r  = getRange c p
    sp = flip scannerPos (step c) <$> r
  in sp == Just 0


getRange :: Config -> Int -> Maybe Int
getRange c p = range <$> Map.lookup p (layout c)


movePos :: Config -> Config
movePos c = c { pos = pos c + 1 }


moveScanner :: Config -> Config
moveScanner c = c { step = step c + 1 }


scannerPos :: Int -> Int -> Int
scannerPos r s = (cycle $ [0..r-1] ++ [r-2,r-3..1]) !! s

----------------------------------------------------------------------
-- input reading/parsing

readInput :: IO Input
readInput = mapMaybe (eval scannerP) . lines <$> readFile "input.txt"


scannerP :: Parser Scanner
scannerP = Scanner <$> layerP <*> rangeP
  where
    layerP = parseInt <* parseString ": "
    rangeP = parseInt


example :: Input
example = [ Scanner 0 3, Scanner 1 2, Scanner 4 4, Scanner 6 4]
