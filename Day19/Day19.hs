-- Solution to Day 19 of the Advent Of Code 2017
{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (elemIndex, (\\))
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Map.Strict as M
import Data.Char (isLetter)
import Debug.Trace


main :: IO ()
main = do
  inp <- readInput
  let map = parseInput inp
      (Just c) = findEntry map
      txt = follow map (0,1) c
  putStrLn $ "part1: " ++ txt


type Input = [T.Text]
type Map   = M.Map Int (M.Map Int Char)
type Coord = (Int, Int)
type Direction = (Int,Int)


findEntry :: Map -> Maybe Coord
findEntry map =
  (,0) <$> (elemIndex '|' . M.elems $ map M.! 0)


follow :: Map -> Direction -> Coord -> [Char]
follow map d@(dx,dy) c@(x,y) =
  let c' = (x+dx,y+dy)
  in case getAt map c' of
       Nothing -> []
       Just c ->
         let rest = fromMaybe [] $ ((\d' -> follow map d' c') <$> direction map d c')
         in if isLetter c then c : rest else rest


direction :: Map -> Direction -> Coord -> Maybe Direction
direction map dir@(dx,dy) c@(x,y) =
  case (map M.! y) M.! x of
    '|' -> Just dir
    '-' -> Just dir
    '+' -> findTurn map c $ neighbours (x,y) \\ [(x-dx,y-dy)]
    ch | isLetter ch -> Just dir
       | otherwise   -> Nothing


neighbours :: Coord -> [Coord]
neighbours (x,y) = [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]


findTurn :: Map -> Coord -> [Coord] -> Maybe Direction
findTurn map _ [] = Nothing
findTurn map (x,y) (c@(x',y'):cs) = do
  ch <- getAt map c
  if ch `elem` ['|','-'] || isLetter ch
    then Just (x'-x,y'-y)
    else findTurn map (x,y) cs


getAt :: Map -> Coord -> Maybe Char
getAt map (x,y) =
  y `M.lookup` map >>= \map' -> x `M.lookup` map'


readInput :: IO Input
readInput = T.lines <$> TIO.readFile "input.txt"


parseInput :: Input -> Map
parseInput = M.fromList . zip [0..] . map parseLine
  where parseLine = M.fromList . zip [0..] . T.unpack
