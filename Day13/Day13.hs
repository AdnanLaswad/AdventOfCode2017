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
  putStrLn $ "part 2: " ++ show (part2 inp)


part1 :: Input -> Int
part1 inp =
  let c = init 0 inp
  in sum . map fst $ game c


part2 :: Input -> Int
part2 inp = head $ valids inp


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


----------------------------------------------------------------------
-- modeling for part 1

game :: Config -> [(Int, Config)]
game conf =
  let l = gameSize conf
  in scanl' go (0, conf) (replicate (l+1) ())
  where
    go (_,c) () = round c


init :: Int -> Input -> Config
init delay inp = Config m delay (-1)
  where m = Map.fromList [ (layer s, s) | s <- inp ]


gameSize :: Config -> Int
gameSize = maximum . map layer . Map.elems . layout


round :: Config -> (Int, Config)
round c =
  let stepIn   = movePos c
      depth    = pos stepIn
      range    = fromMaybe 0 $ getRange c (pos stepIn)
      caugth   = isCaught stepIn
      severity = if caugth then depth * range else 0
      nextC    = moveScanner stepIn
  in (severity, nextC)


isCaught :: Config -> Bool
isCaught c =
  let
    p  = pos c
    r  = getRange c p
    sp = (step c `mod`) . cycleLength <$> r
  in sp == Just 0


getRange :: Config -> Int -> Maybe Int
getRange c p = range <$> Map.lookup p (layout c)


movePos :: Config -> Config
movePos c = c { pos = pos c + 1 }


moveScanner :: Config -> Config
moveScanner c = c { step = step c + 1 }


cycleLength :: Range -> Int
cycleLength r = 2 * r - 2


----------------------------------------------------------------------
-- mathy part 2 because of optimization

valids :: [Scanner] -> [Int]
valids = orderedDiff [0..] . invalids


invalids :: [Scanner] -> [Int]
invalids = foldr merge [] . map invalidDelays


invalidDelays :: Scanner -> [Int]
invalidDelays sc = [fst,fst + cyc..]
  where fst = (negate $ layer sc) `mod` cyc
        cyc = cycleLength $ range sc


merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge xs@(x:xs') ys@(y:ys')
  | x == y    = x : merge xs' ys'
  | x <  y    = x : merge xs' ys
  | otherwise = y : merge xs ys'


orderedDiff :: Ord a => [a] -> [a] -> [a]
orderedDiff [] _  = []
orderedDiff xs [] = xs
orderedDiff xs@(x:xs') ys@(y:ys')
  | x == y    = orderedDiff xs' ys'
  | x <  y    = x : orderedDiff xs' ys
  | otherwise = orderedDiff xs ys'

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



