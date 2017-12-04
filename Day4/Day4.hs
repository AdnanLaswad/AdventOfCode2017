-- Solution to Day 4 of the Advent Of Code 2017

module Main where

import Data.List (sort, group, nub)

main :: IO ()
main = do
  inp <- readInput

  let part1 = length . filter noDuplicateWords $ inp
  putStrLn $ "part 1: " ++ show part1

  let part2 = length . filter isValidPassphrase $ inp
  putStrLn $ "part 2: " ++ show part2



type Input = [String] 


isValidPassphrase :: [String] -> Bool
isValidPassphrase xs = noDuplicateWords xs && noAnagrams xs


noAnagrams :: [String] -> Bool
noAnagrams xs = length xs == length (nub $ map sort xs)


noDuplicateWords :: [String] -> Bool
noDuplicateWords = all ((== 1) . length) . group . sort


readInput :: IO [Input]
readInput = map words . lines <$> readFile "input.txt"
