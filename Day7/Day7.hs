-- Solution to Day 7 of the Advent Of Code 2017

module Main where

import Data.Function (on)
import Data.List (foldl', groupBy, sortBy)
import Data.Maybe (mapMaybe, fromJust)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import System.IO (hSetBuffering, BufferMode(..), stdout, stdin)

import Parser


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering

  putStr "read input..."
  inp <- readInput
  putStrLn "done"

  let [base] = findBase inp

  putStrLn $ "part1: " ++ base

  let m = buildMap inp
      unb = findUnbalance m base Nothing

  putStrLn $ "part2: " ++ show unb


----------------------------------------------------------------------
-- part 1

findBase :: Input -> [Name]
findBase inp =
  let children = concatMap supporting inp
  in filter (not . (`elem` children)) $ map name inp


----------------------------------------------------------------------
-- part 2

findUnbalance :: Map Name Program -> Name -> Maybe Int -> Int
findUnbalance m b targetWeight =
  case Map.lookup b m of
    Nothing -> error "base not found"
    Just p ->
      let ws = (\n -> (n, weightTree m n)) <$> supporting p
          grps = sortBy (compare `on` length) . groupBy ((==) `on` snd) $ sortBy (compare `on` snd) ws
      in case grps of
           -- if there are no children then the node itself is the wrong weight
           -- there should better be a targetWeight!
           []                    -> fromJust targetWeight
           -- if all children have the same weight then we have to adapt the node weight
           -- again targetWeigth is neccessary
           [xs]                  -> let supW = sum (snd <$> xs) in fromJust targetWeight - supW
           -- if there are only two children then the good one must be half the remaining targetWeight
           -- if there is no targetValue yet we are out of luck
           [[(ca,wa)],[(cb,wb)]]
             | Just wa == targetChildWeight p -> findUnbalance m cb (Just wa)
             | Just wb == targetChildWeight p -> findUnbalance m ca (Just wb)
             | otherwise                      -> error "don't know which way to go - shitty config man"
           -- if there is one child that has a different weight to all the others we
           -- have to adapt it's weight to be the same as the other childrens weight
           [[(cont,_)], (_,w):_] -> findUnbalance m cont (Just w)
           -- in all other cases the input have to be malformed
           _ -> error ("shitty config at " ++ b)
  where
    targetChildWeight p = fmap (\tw -> (tw - weight p)`div` 2) targetWeight


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
