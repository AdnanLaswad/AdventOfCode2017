module Main where

import Data.Maybe (catMaybes)
import Text.Read (readMaybe)

main :: IO ()
main = do
  inp <- readInput
  let sol = solution inp
  print sol
  let sol2 = solution2 inp
  print sol2


solution :: [Integer] -> Integer
solution xs = sum . catMaybes . map goodPairs $ pairs (xs ++ [head xs])
  where goodPairs (a,b)
          | a == b    = Just a
          | otherwise = Nothing


solution2 :: [Integer] -> Integer
solution2 xs = sum . catMaybes . map goodPairs $ zipWith (,) xs (drop mid xs ++ take mid xs)
  where goodPairs (a,b)
          | a == b    = Just a
          | otherwise = Nothing
        mid = length xs `div` 2

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs xs = zipWith (,) xs (tail xs)


readInput :: IO [Integer]
readInput = catMaybes . map (readMaybe . pure) <$> readFile "input.txt"


