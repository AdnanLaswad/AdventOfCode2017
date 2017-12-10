-- Solution to Day 10 of the Advent Of Code 2017

module Main where


main :: IO ()
main = do
  inp <- readInput

  putStrLn $ "part 1: " ++ show (part1 inp)


type Input = [Int]


part1 :: Input -> Int
part1 inp =
  let (a:b:_) = steps [0..255] inp
  in a * b


steps :: [Int] -> Input -> [Int]
steps list inp = go 0 (0, list) inp
  where
    go sk (pos, xs) [] = xs
    go sk (pos, xs) (l:ls) = go (sk+1) (step len sk l (pos, xs)) ls
    len = length list


step :: Int -> Int -> Int -> (Int, [Int]) -> (Int, [Int])
step listLen skip l (cur, list) = (cur', list')
  where
    (end, start)  = splitAt (listLen - cur) $ take listLen $ reverse sel ++ notSel
    (sel, notSel) = splitAt l $ drop cur $ list ++ list
    list' = start ++ end
    cur' = (cur + skip + l) `mod` listLen


readInput :: IO Input
readInput = map read . split <$> readFile "input.txt"


split :: String -> [String]
split = go []
  where
    go acc []         = if null acc then [] else [reverse acc]
    go acc (',':rest) = reverse acc : go [] rest
    go acc (c  :rest) = go (c:acc) rest
