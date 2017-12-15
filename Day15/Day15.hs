-- Solution to Day 15 of the Advent Of Code 2017

module Main where


import Data.Bits (Bits, (.&.))
import Data.Word (Word16)

main :: IO ()
main = do
  let inp = problemInput
  putStrLn $ "part1: " ++ show (run (0,0) 40000000 inp)
  putStrLn $ "part2: " ++ show (run (3,7) 5000000 inp)


type Input = (Int, Int)


problemInput :: Input
problemInput = (883, 879)


exampleInput :: Input
exampleInput = (65, 8921)


factorA :: Num a => a
factorA = 16807

factorB :: Num a => a
factorB = 48271


dividend :: Num a => a
dividend = 2147483647


run :: (Int, Int) -> Int -> Input -> Int
run (multA, multB) n inp = sum $ take n $ judgeSeq (multA, multB) inp


judgeSeq :: (Int, Int) -> Input -> [Int]
judgeSeq (multA, multB) (startA, startB) =
  zipWith judge
  (tail $ generate factorA multA startA)
  (tail $ generate factorB multB startB)


judge :: (Integral a) => a -> a -> a
judge a b = if last16Bit a == last16Bit b then 1 else 0


generate :: (Bits a, Integral a) => a -> a -> a -> [a]
generate factor mult start = filter (noLastBits mult) $ iterate (genFunction factor) start


noLastBits :: (Num a, Bits a) => a -> a -> Bool
noLastBits mask n = n .&. mask == 0


genFunction :: Integral a => a -> a -> a
genFunction factor last =
  (last * factor) `mod` dividend


last16Bit :: (Integral a) => a -> Word16
last16Bit = fromIntegral
