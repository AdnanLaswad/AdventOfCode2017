-- Solution to Day 12 of the Advent Of Code 2017

module Main where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map

import Data.IntSet (IntSet, (\\))
import qualified Data.IntSet as Set

import Data.Maybe (mapMaybe, fromMaybe)

import Parser


main :: IO ()
main = do
  inp <- readInput
  let g = buildGraph inp

  putStrLn $ "part 1: " ++ show (part1 g)
  putStrLn $ "part 2: " ++ show (part2 g)


----------------------------------------------------------------------
-- data and types

type Input = [(Id, [Id])]

type Connection = (Id, Id)

type Id = Int

type Graph = IntMap [Id]

type Nodes = IntSet

type Group = IntSet


----------------------------------------------------------------------
-- solutions

part1 :: Graph -> Int
part1 g = Set.size $ epsClosure g 0


part2 :: Graph -> Int
part2 g = length $ groups g


----------------------------------------------------------------------
-- graph functions

groups :: Graph -> [Group]
groups g = go (nodes g)
  where
    go ns =
      if Set.null ns
      then []
      else
        let gr = epsClosure g (Set.findMin ns)
        in  gr : go (ns \\ gr)


nodes :: Graph -> Nodes
nodes = Set.fromList . Map.keys


epsClosure :: Graph -> Id -> Nodes
epsClosure graph i = go Set.empty [i]
  where
    go visited [] = visited
    go visited (i:is) =
      if i `Set.member` visited
      then go visited is
      else go (Set.insert i visited) (is ++ connections graph i)


connections :: Graph -> Id -> [Id]
connections g i = fromMaybe [] $ Map.lookup i g


buildGraph :: Input -> Graph
buildGraph inp = foldr insertCon emptyGraph [ (f,t) | (f,ts) <- inp, t <- ts ]


emptyGraph :: Graph
emptyGraph = Map.empty


insertCon :: Connection -> Graph -> Graph
insertCon (from,to) =
  Map.insertWith (++) from [to]
  . Map.insertWith (++) to [from]


----------------------------------------------------------------------
-- parse input

readInput :: IO Input
readInput = mapMaybe (eval lineP) . lines <$> readFile "input.txt"


lineP :: Parser (Id, [Id])
lineP = (,) <$> idP <*> listP


idP :: Parser Id
idP = parseInt <* ignoreWhiteSpace

listP :: Parser [Id]
listP = parseString "<->" *> ignoreWhiteSpace *>
        (parseInt `parseSepBy` parseString ", ")
