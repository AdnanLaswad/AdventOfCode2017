module Main where

import Data.Maybe (mapMaybe, catMaybes)
import Text.Read (readMaybe)

main :: IO ()
main = do
  inp <- readInput
  let sol = solution inp
  putStrLn $ "Solution to part one is " ++ show sol

  let sol2 = solution2 inp
  putStrLn $ "Solution to part two is " ++ show sol2


solution :: [Integer] -> Integer
solution =sumWrap 1


solution2 :: [Integer] -> Integer
solution2 xs = sumWrap mid xs
  where mid = length xs `div` 2


sumWrap :: Int -> [Integer] -> Integer
sumWrap n xs = sumGoodPairs xs (drop n xs ++ take n xs)


sumGoodPairs :: [Integer] -> [Integer] -> Integer
sumGoodPairs xs ys = sum . catMaybes $ zipWith goodPair xs ys
  where goodPair a b
          | a == b    = Just a
          | otherwise = Nothing


readInput :: IO [Integer]
readInput = mapMaybe (readMaybe . pure) <$> readFile "input.txt"


