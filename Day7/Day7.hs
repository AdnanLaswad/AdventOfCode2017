-- Solution to Day 7 of the Advent Of Code 2017

module Main where

import Data.Function (on)
import Data.List (foldl', groupBy, sortBy)
import Data.Maybe (mapMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Parser


main :: IO ()
main = do
  inp <- readInput
  let [base] = findBase inp [] []

  putStrLn $ "part1: " ++ base

  let m = buildMap inp
      unb = findUnbalance m base 0

  putStrLn $ "part2: " ++ show unb


----------------------------------------------------------------------
-- part 1

findBase :: Input -> [Name] -> [Name] -> [Name]
findBase [] onTop canditates = canditates
findBase (p:rest) onTop canditates =
  let canditates' = filter (not . (`elem` supporting p)) canditates
      canditates'' = if name p `elem` onTop then canditates' else name p : canditates'
      onTop' = onTop ++ supporting p
  in findBase rest onTop' canditates''


----------------------------------------------------------------------
-- part 2

findUnbalance :: Map Name Program -> Name -> Int -> Int
findUnbalance m b targetWeight =
  case Map.lookup b m of
    Nothing -> error "base not found"
    Just p ->
      let ws = (\n -> (n, weightTree m n)) <$> supporting p
          grps = sortBy (compare `on` length) . groupBy ((==) `on` snd) $ sortBy (compare `on` snd) ws
      in case grps of
           [] -> targetWeight
           [xs] -> let supW = sum (snd <$> xs) in targetWeight - supW
           [[(cont,_)], (_,w):_] -> findUnbalance m cont w
           _ -> error ("shitty config at " ++ b)


----------------------------------------------------------------------
type Input = [Program]


data Program =
  Program
  { name :: Name
  , weight :: Int
  , supporting :: [Name]
  } deriving Show


type Name = String


----------------------------------------------------------------------
-- helpers

weightTree :: Map Name Program -> Name -> Int
weightTree m n =
  case Map.lookup n m of
    Nothing -> 0
    Just p  -> weight p + sum (weightTree m <$> supporting p)


buildMap :: Input -> Map Name Program
buildMap = foldl' (\m p -> Map.insert (name p) p m) Map.empty


----------------------------------------------------------------------
-- parsing

readInput :: IO Input
readInput = mapMaybe readLine . lines <$> readFile "input.txt"


readLine :: String -> Maybe Program
readLine = eval programP


programP :: Parser Program
programP = Program <$> nameP <*> weightP <*> supP


nameP :: Parser Name
nameP = parseAlphas <* ignoreWhiteSpace


weightP :: Parser Int
weightP = parseChar (== '(') *> parseInt <* parseChar (== ')') <* ignoreWhiteSpace


supP :: Parser [Name]
supP = parseEither (parseString "->" *> ignoreWhiteSpace *> supListP) (pure [])


supListP :: Parser [Name]
supListP = do
  item <- nameP
  rest <- parseEither (parseChar (== ',') *> ignoreWhiteSpace *> supListP) (pure [])
  return $ item : rest


exampleLine :: String
exampleLine = "fwft (72) -> ktlj, cntj, xhth"
