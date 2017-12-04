-- Solution to Day 4 of the Advent Of Code 2017

module Main where

import Data.List (sort, group)

main :: IO ()
main = do
  inp <- readInput
  
  let part1 = length . filter isValidPassphrase $ inp
  putStrLn $ "part 1: " ++ show part1


type Input = [String] 


isValidPassphrase :: [String] -> Bool
isValidPassphrase = all ((== 1) . length) . group . sort


readInput :: IO [Input]
readInput = map words . lines <$> readFile "input.txt"
