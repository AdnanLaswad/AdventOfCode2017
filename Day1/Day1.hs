module Main where

import Data.Maybe (catMaybes)
import Text.Read (readMaybe)

main :: IO ()
main = do
  inp <- readInput
  let sol = solution inp
  print sol


solution :: [Integer] -> Integer
solution xs = sum . catMaybes . map goodPairs $ pairs (xs ++ [head xs])
  where goodPairs (a,b)
          | a == b    = Just a
          | otherwise = Nothing


pairs :: [a] -> [(a,a)]
pairs [] = []
pairs xs = zipWith (,) xs (tail xs)


readInput :: IO [Integer]
readInput = catMaybes . map (readMaybe . pure) <$> readFile "input.txt"


