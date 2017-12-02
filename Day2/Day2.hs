-- Solution to Day 2 of the Advent Of Code 2017

module Main where


main :: IO ()
main = do
  inp <- readInput

  let part1 = checksum lineValue inp
  putStrLn $ "Solution to part 1 is " ++ show part1

  let part2 = checksum lineValue2 inp
  putStrLn $ "Solution to part 2 is " ++ show part2


-- | read in the input consisting of multiple lines
--   each line contains numbers seperated by spaces
readInput :: IO [[Int]]
readInput = (map (map read . words) . lines) <$> readFile "input.txt"


-- | each part asks to find some kind of chekcsum
--   that is always the sum of some numbers calculated for each line
checksum :: ([Int] -> Int) -> [[Int]] -> Int
checksum alg = sum . map alg


-- | part 1 just looks for the differnce of
--   the largest and smalest number in line
lineValue :: [Int] -> Int
lineValue ns = maximum ns - minimum ns


-- | parts 2 looks for a single factor
lineValue2 :: [Int] -> Int
lineValue2 = sum . map factor . pickTwo
  where
    factor (a,b)
      | a < b          = factor (b,a) 
      | a `mod` b == 0 = a `div` b
      | otherwise      = 0


----------------------------------------------------------------------
-- helpers

pickTwo :: [a] -> [(a,a)]
pickTwo xs = do
  (a, xs') <- pick xs
  (b, _)   <- pick xs'
  return (a,b)


pick :: [a] -> [(a,[a])]
pick [] = []
pick (x:xs) = (x,xs) : [ (y, ys) | (y,ys) <- pick xs ]
