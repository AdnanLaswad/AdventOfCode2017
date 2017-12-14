module Graph where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Set (Set, (\\))
import qualified Data.Set as Set

import Data.Maybe (mapMaybe, fromMaybe)

----------------------------------------------------------------------
-- data and types

type Connection n w = (n, n, w)

data Node n w =
  Node
  { nodeId :: n
  , nodeConnections :: [(w, n)]
  } deriving Show


type Graph n w = Map n (Node n w)

type Nodes n = Set n

type Group n = Set n

----------------------------------------------------------------------
-- graph functions

groups :: Ord n => Graph n w -> [Group n]
groups g = go (nodes g)
  where
    go ns =
      if Set.null ns
      then []
      else
        let gr = epsClosure g (Set.findMin ns)
        in  gr : go (ns \\ gr)


nodes :: Ord n => Graph n w -> Nodes n
nodes = Set.fromList . Map.keys


epsClosure :: Ord n => Graph n w -> n -> Nodes n
epsClosure graph i = go Set.empty [i]
  where
    go visited [] = visited
    go visited (i:is) =
      if i `Set.member` visited
      then go visited is
      else go (Set.insert i visited) (is ++ connections graph i)


connections :: Ord n => Graph n w -> n -> [n]
connections g i =
  fromMaybe []
  $ fmap (fmap snd . nodeConnections)
  $ Map.lookup i g


emptyGraph :: Graph n w
emptyGraph = Map.empty


insertCon :: Ord n => Connection n w -> Graph n w -> Graph n w
insertCon (from, to, weight) =
  Map.insertWith addCon from (Node from [(weight,to)])
  . Map.insertWith addCon to (Node to [(weight,from)])
  where
    addCon n (Node _ cons) = n { nodeConnections = nodeConnections n ++ cons }
