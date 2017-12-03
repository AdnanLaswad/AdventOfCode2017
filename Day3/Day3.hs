-- Solution to Day 3 of the Advent Of Code 2017

module Main where

import Prelude hiding (Either(..), init)
import Data.List (unfoldr)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

type Input = ()


keyPart1 :: Int
keyPart1 = 312051


main :: IO ()
main = do
  putStrLn $ "part 1: " ++ show (solve keyPart1)
  putStrLn $ "part 2: " ++ show solve2

solve :: Int -> Int
solve key = head $ drop (key-1) distSnake


distSnake :: [Int]
distSnake = (0:) . concat $ unfoldr
  (\ xs ->
     let ys = nextRingSide xs
     in Just (concat (replicate 4 $ tail ys), ys))
  [0]


nextRingSide :: [Int] -> [Int]
nextRingSide xs@(x:_) = (x + 2) : map (+1) xs ++ [x+2]


----------------------------------------------------------------------
-- part 2

solve2 :: Int
solve2 = head $ dropWhile (<= keyPart1) values


----------------------------------------------------------------------
-- calculate spiral coords

data Dir = Up | Left | Down | Right
  deriving Show


coords :: [(Int,Int)]
coords = scanl (flip apply) (0,0) moves


moves :: [Dir]
moves = concatMap ((Right:) . tail . layerMoves) [1..]


layerMoves :: Int -> [Dir]
layerMoves 0 = []
layerMoves n =
  concatMap (replicate len) [Up, Left, Down, Right]
  where len = 2 * n


apply :: Dir -> (Int,Int) -> (Int,Int)
apply Right (x,y) = (x+1,y)
apply Left  (x,y) = (x-1,y)
apply Up    (x,y) = (x,y-1)
apply Down  (x,y) = (x,y+1)


----------------------------------------------------------------------
-- calculate values

type Env = Map (Int,Int) Int


values :: [Int]
values = map fst $ scanl (\ (_, env) coord -> step env coord) (1,init)$ tail coords


step :: Env -> (Int,Int) -> (Int, Env)
step env coord =
  let value = addNeighbours env coord
  in (value, Map.insert coord value env)


init :: Env
init = Map.fromList $ pure ((0,0), 1)


addNeighbours :: Env -> (Int,Int) -> Int
addNeighbours env (x,y) =
  sum $ map (getValue env) $ neighbours (x,y)


neighbours :: (Int,Int) -> [(Int,Int)]
neighbours (x,y) =
  [ (x+1,y),(x+1,y-1)
  , (x,y-1),(x-1,y-1)
  , (x-1,y),(x-1,y+1)
  , (x,y+1),(x+1,y+1)
  ]


getValue :: Env -> (Int,Int) -> Int
getValue env coord =
  fromMaybe 0 $ Map.lookup coord env
