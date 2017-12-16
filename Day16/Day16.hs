-- Solution to Day 16 of the Advent Of Code 2017
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T

import Parser

main :: IO ()
main = do
  inp <- readInput

  let part1 = dance inp start
  putStrLn $ "part1: " ++ T.unpack part1

  putStrLn $ "part2: " ++ T.unpack (part2 inp)


----------------------------------------------------------------------
-- data and types

type Input = [DanceMove]

type Line = Text

data DanceMove
  = Spin Int
  | Exchange Int Int
  | Swap Char Char
  deriving Show


----------------------------------------------------------------------
-- part 2

part2 :: Input -> Line
part2 mvs =
  let cl = cycleLength start mvs
      times = 10000000 `mod` cl
  in nTimes times (dance mvs) start


cycleLength :: Line -> [DanceMove] -> Int
cycleLength s mvs = go s
  where go x =
          let x' = dance mvs x
          in if x' == s then 1 else 1 + go x'


----------------------------------------------------------------------
-- part 1

dance :: [DanceMove] -> Line -> Line
dance = flip (foldl' (flip move))


move :: DanceMove -> Line -> Line
move (Spin n) line   = end `T.append` start
  where (start, end) = T.splitAt (T.length line - n) line
move (Exchange i j) line
  | i == j     = line
  | i >  j     = move (Exchange j i) line
  | otherwise =
    let (start, mid) = T.splitAt i line
        (mid', end)  = T.splitAt (j-i) mid
        (ci,mid'')   = fromJust $ T.uncons mid'
        (cj,end')    = fromJust $ T.uncons end
    in T.concat [start, T.cons cj mid'', T.cons ci end']
move (Swap a b) line = T.map swap line
  where swap c
          | c == a = b
          | c == b = a
          | otherwise = c


start :: Line
start = T.pack ['a'..'p']


---------------------------------------------------------------------
-- helpers

nTimes :: Int -> (a -> a) -> a -> a
nTimes 0 _ !x = x
nTimes n f !x = nTimes (n-1) f (f x)


----------------------------------------------------------------------
-- input and parsing

readInput :: IO Input
readInput = fromJust . eval movesP <$> readFile "input.txt"


movesP :: Parser [DanceMove]
movesP = moveP `parseSepBy` (parseChar ',')

moveP :: Parser DanceMove
moveP = parseOneOf [ spinP, exchangeP, swapP ]


spinP :: Parser DanceMove
spinP = Spin <$> (parseChar 's' *> parseInt)


exchangeP :: Parser DanceMove
exchangeP = Exchange <$> (parseChar 'x' *> parseInt) <*> (parseChar '/' *> parseInt)


swapP :: Parser DanceMove
swapP = Swap <$> (parseChar 'p' *> parseAlpha) <*> (parseChar '/' *> parseAlpha)
