-- Solution to Day 24 of the Advent Of Code 2017

module Main where

import Data.Function (on)
import Data.Maybe (fromJust, fromMaybe)
import Data.List (maximumBy)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Parser


main :: IO ()
main = do
  inp <- readInput

  putStrLn $ "part 1: " ++ show (part1 inp)


type Input = [Component]

type Stash = IM.IntMap [Port]

type Component = (Port, Port)

type Port = Int

type Chain = [Component]


part1 :: Input -> Int
part1 = maximum . map strength . chains . createStash


strongest :: [Chain] -> Chain
strongest = maximumBy (compare `on` strength)


strength :: Chain -> Int
strength = sum . map (uncurry (+)) 


chains :: Stash -> [Chain]
chains stash = map snd $ go (stash, 0)
  where
    go (st, from) =
      let tos = fromMaybe [] $ IM.lookup from st
      in if null tos
         then return (st, [])
         else do
            to <- tos
            let st' = removeFromStash (from,to) st
            (st'', rest) <- go (st', to)
            return (st'', (from,to) : rest)


createStash :: Input -> Stash
createStash = foldr addToStash IM.empty


addToStash :: Component -> Stash -> Stash
addToStash (a,b) = IM.insertWith (++) a [b] . IM.insertWith (++) b [a]


removeFromStash :: Component -> Stash -> Stash
removeFromStash (a,b) =
  IM.insertWith (\ _ -> delete b) a [] . IM.insertWith (\ _ -> delete a) b []


delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete x (y:ys)
  | x == y = ys
  | otherwise = y : delete x ys


pickOne :: [a] -> [(a,[a])]
pickOne [] = []
pickOne (a:as) = (a,as) : [ (y,a:ys) | (y,ys) <- pickOne as ]


testInput :: Input
testInput = [ (0,2), (2,2), (2,3), (3,4), (3,5), (0,1), (10,1), (9,10) ]


readInput :: IO Input
readInput = map (fromJust . eval componentP) . lines <$> readFile "input.txt"


componentP :: Parser Component
componentP = (,) <$> portP <* parseString "/" <*> portP


portP :: Parser Port
portP = parseInt
