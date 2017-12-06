-- Solution to Day 6 of the Advent Of Code 2017

module Main where


import Data.Function (on)
import Data.List (elemIndex)


main :: IO ()
main = do
  inp <- readInput
  let (part1, b') = steps inp
  putStrLn $ "answer part1: " ++ show part1

  let (part2, _) = steps b'
  putStrLn $ "answer part2: " ++ show part2


type Input = Banks

type Banks = [Block]

type Block = Int


readInput :: IO Input
readInput = map read . words <$> readFile "input.txt"


example :: Banks
example = [0,2,7,0]


steps :: Banks -> (Int, Banks)
steps banks = findDouble banks []
  where
    findDouble b seen =
      if b `elem` seen then (0, b) else let (n,b') = findDouble (step b) (b:seen) in (n+1, b')


step :: Banks -> Banks
step banks =
  let max = maximum banks
      (Just ind) = elemIndex max banks
      banks' = take ind banks ++ 0 : drop (ind+1) banks
  in spread banks' (ind+1) max


spread :: Banks -> Int -> Int -> Banks
spread banks start n =
  let parts = chunksOf l $ replicate start 0 ++ replicate n 1 ++ replicate ( negate (start + n) `mod` l ) 0
      l   = length banks
  in foldr (zipWith (+)) banks parts


chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)
