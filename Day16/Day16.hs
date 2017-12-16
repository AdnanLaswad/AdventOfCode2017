-- Solution to Day 16 of the Advent Of Code 2017
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List (foldl')
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T

import Parser

main :: IO ()
main = do
  inp <- readInput

  let part1 = dance inp
  putStrLn $ "part1: " ++ T.unpack part1


type Input = [DanceMove]

type Line = Text

data DanceMove
  = Spin Int
  | Exchange Int Int
  | Swap Char Char
  deriving Show


start :: Line
start = T.pack ['a'..'p']


exampleStart :: Line
exampleStart = T.pack ['a'..'e']


dance :: [DanceMove] -> Line
dance = foldl' (flip move) start


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
