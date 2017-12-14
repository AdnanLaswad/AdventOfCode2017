-- Solution to Day 14 of the Advent Of Code 2017

module Main where

import Data.Function (on)
import Data.List (foldl', sort, group)
import Data.Maybe (fromMaybe, mapMaybe, fromJust)
import Data.Bits (xor)
import Data.Char (ord)
import Data.List (foldl1')
import Text.Printf (printf)


import Parser


key :: Input
key = "ffayrhll"



main :: IO ()
main = do
  let grid = initGrid key
  putStrLn $ "part 1: " ++ show (part1 grid)


type Input = String


type Grid = [String]


part1 :: Grid -> Int
part1 = sum . map (length . filter (== '1'))


initGrid :: Input -> Grid
initGrid key = [ concatMap charToFrag (knotHash (key ++ "-" ++ show row)) | row <- [0..127] ]


----------------------------------------------------------------------
-- hash algorithm

knotHash :: String -> String
knotHash inp = concatMap (printf "%02x") hashedBlocks
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


charToFrag :: Char -> String
charToFrag '0' = "0000"
charToFrag '1' = "0001"
charToFrag '2' = "0010"
charToFrag '3' = "0011"
charToFrag '4' = "0100"
charToFrag '5' = "0101"
charToFrag '6' = "0110"
charToFrag '7' = "0111"
charToFrag '8' = "1000"
charToFrag '9' = "1001"
charToFrag 'a' = "1010"
charToFrag 'b' = "1011"
charToFrag 'c' = "1100"
charToFrag 'd' = "1101"
charToFrag 'e' = "1110"
charToFrag 'f' = "1111"
