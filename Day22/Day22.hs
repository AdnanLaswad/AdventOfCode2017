-- Solution to Day 22 of the Advent Of Code 2017

module Main where


main :: IO ()
main = do
  inp <- readInput
  putStrLn $ "solve me"


type Input = ()


readInput :: IO Input
readInput = const () <$> readFile "input.txt"
