-- Solution to Day 3 of the Advent Of Code 2017

module Main where

import Data.List (unfoldr)

type Input = ()


keyPart1 :: Int
keyPart1 = 312051


main :: IO ()
main = do
  putStrLn $ "part 1: " ++ show (solve keyPart1)

solve :: Int -> Int
solve key = head $ drop (key-1) distSnake


nextRingSide :: [Int] -> [Int]
nextRingSide xs@(x:_) = (x + 2) : map (+1) xs ++ [x+2]


distSnake :: [Int]
distSnake = (0:) . concat $ unfoldr
  (\ xs ->
     let ys = nextRingSide xs
     in Just (concat (replicate 4 $ tail ys), ys))
  [0]


readInput :: IO Input
readInput = parse <$> readFile "input.txt"
  where parse = error "implement me"
