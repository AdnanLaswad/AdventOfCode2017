-- Solution to Day 12 of the Advent Of Code 2017

module Main where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map

import Data.IntSet (IntSet, (\\))
import qualified Data.IntSet as Set

import Data.Maybe (mapMaybe, fromMaybe)

import Graph
import Parser


main :: IO ()
main = do
  g <- readGraph

  putStrLn $ "part 1: " ++ show (part1 g)
  putStrLn $ "part 2: " ++ show (part2 g)


----------------------------------------------------------------------
-- data and types

type Input = [(Id, [Id])]


----------------------------------------------------------------------
-- solutions

part1 :: Graph -> Int
part1 g = Set.size $ epsClosure g 0


part2 :: Graph -> Int
part2 g = length $ groups g


----------------------------------------------------------------------
-- graph functions

buildGraph :: Input -> Graph
buildGraph inp = foldr insertCon emptyGraph [ (f,t) | (f,ts) <- inp, t <- ts ]


----------------------------------------------------------------------
-- parse input

readGraph :: IO Graph
readGraph = buildGraph <$> readInput


readInput :: IO Input
readInput = mapMaybe (eval lineP) . lines <$> readFile "input.txt"


lineP :: Parser (Id, [Id])
lineP = (,) <$> idP <*> listP


idP :: Parser Id
idP = parseInt <* ignoreWhiteSpace


listP :: Parser [Id]
listP = parseString "<->" *> ignoreWhiteSpace *>
        (parseInt `parseSepBy` parseString ", ")
