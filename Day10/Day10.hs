-- Solution to Day 10 of the Advent Of Code 2017

module Main where

import Data.Bits (xor)
import Data.Char (ord)
import Data.List (foldl1')
import Text.Printf (printf)

main :: IO ()
main = do
  inp <- readInput

  putStrLn $ "part 1: " ++ show (part1 $ map read $ split inp)
  putStrLn $ "part 2: " ++ part2 inp


----------------------------------------------------------------------
-- algorithm

part1 :: [Int] -> Int
part1 inp =
  let (a:b:_) = steps [0..255] inp
  in a * b


part2 :: String -> String
part2 inp = concatMap (printf "%02x") hashedBlocks
  where inp' = concat . replicate 64 $ final inp
        step1 = steps [0..255] inp'
        hashedBlocks = hash <$> blocks step1


hash :: [Int] -> Int
hash = foldl1' xor


blocks :: [a] -> [[a]]
blocks [] = []
blocks xs = let (b, xs') = splitAt 16 xs in b : blocks xs'


final :: String -> [Int]
final inp = map ord  inp ++ [17, 31, 73, 47, 23]


steps :: [a] -> [Int] -> [a]
steps list = go 0 (0, list)
  where
    go _ (_, xs) []        = xs
    go sk (pos, xs) (l:ls) = go (sk+1) (step len sk l (pos, xs)) ls
    len                    = length list


step :: Int -> Int -> Int -> (Int, [a]) -> (Int, [a])
step listLen skip l (cur, list) = (cur', list')
  where
    (end, start)  = splitAt (listLen - cur) $ take listLen $ reverse sel ++ notSel
    (sel, notSel) = splitAt l $ drop cur $ list ++ list
    list' = start ++ end
    cur' = (cur + skip + l) `mod` listLen


----------------------------------------------------------------------
-- read input

readInput :: IO String
readInput =
  -- remove the trailing '\n'
  head . lines <$> readFile "input.txt"


split :: String -> [String]
split = (uncurry (:)) . foldr f ([],[])
  where
    f ',' (acc, ls) = ([], acc:ls)
    f c   (acc, ls) = (c:acc, ls)
