-- Solution to Day 24 of the Advent Of Code 2017

module Main where

import Data.Function (on)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (maximumBy, sortBy, groupBy)
import Data.Maybe (fromJust, fromMaybe)
import Parser


main :: IO ()
main = do
  inp <- readInput

  putStrLn $ "part 1: " ++ show (part1 inp)
  putStrLn $ "part 2: " ++ show (part2 inp)


type Input = [Component]

type Stash = IM.IntMap IS.IntSet

type Component = (Port, Port)

type Port = Int

type Chain = [Component]


part1 :: Input -> Int
part1 = maximum . map strength . chains . createStash


part2 :: Input -> Int
part2 =
  maximum . map strength . head . groupBy ((==) `on` chainLength) . sortBy (flip compare `on` chainLength) . chains . createStash


strongest :: [Chain] -> Chain
strongest = maximumBy (compare `on` strength)


strength :: Chain -> Int
strength = sum . map (uncurry (+))


chainLength :: Chain -> Int
chainLength = length


chains :: Stash -> [Chain]
chains stash = map snd $ go (stash, 0)
  where
    go (st, from) =
      let tos = fromMaybe [] . fmap IS.toList $ IM.lookup from st
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
addToStash (a,b) = IM.insertWith IS.union a (IS.singleton b) . IM.insertWith IS.union b (IS.singleton a)


removeFromStash :: Component -> Stash -> Stash
removeFromStash (a,b) =
  IM.insertWith (\ _ -> IS.delete b) a IS.empty . IM.insertWith (\ _ -> IS.delete a) b IS.empty


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
