-- Solution to Day 9 of the Advent Of Code 2017

module Main where

import Data.Maybe (fromJust)
import Parser



main :: IO ()
main = do
  inp <- readInput

  let part1 = score 1 inp
  putStrLn $ "part 1: " ++ show part1

  let part2 = countGarbageChars inp
  putStrLn $ "part 2: " ++ show part2


----------------------------------------------------------------------
-- data and types

type Input = Group


data Group =
  Group [Content]
  deriving Show


data Content
  = Inner Group
  | Garbage String
  deriving Show


----------------------------------------------------------------------
-- algorithm

score :: Int -> Group -> Int
score s (Group cs) = s + sum (map (scoreContent $ s) cs)


scoreContent :: Int -> Content -> Int
scoreContent outer (Inner g)   = score (outer+1) g
scoreContent _     (Garbage _) = 0


countGarbageChars :: Group -> Int
countGarbageChars (Group cs) = sum $ map countGarbageChars' cs
  where countGarbageChars' (Inner g)    = countGarbageChars g
        countGarbageChars' (Garbage g)  = length g


----------------------------------------------------------------------
-- inpt and parsing

readInput :: IO Input
readInput = fromJust . eval groupP <$> readFile "input.txt"


groupP :: Parser Group
groupP = Group <$> (parseBetween (oneP '{') (oneP '}') $ contentP `parseSepBy` (oneP ','))


contentP :: Parser Content
contentP = parseEither (Inner <$> groupP) (Garbage <$> garbageP)


garbageP :: Parser String
garbageP = parseBetween (oneP '<') (oneP '>') (parseMany $ allButP '>')


allButP :: Char -> Parser Char
allButP c = do
  c' <- charP
  if c' /= c then return c' else failParse


oneP :: Char -> Parser ()
oneP c = do
  c' <- charP
  if c' == c then return () else failParse


charP :: Parser Char
charP = do
  c <- parsePred (const True)
  if c == '!' then do
    _ <- parsePred (const True)
    charP
  else
    return c
