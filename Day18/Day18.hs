-- Solution to Day 18 of the Advent Of Code 2017

module Main where

import Data.Char (isLetter)
import qualified Data.IntMap.Strict as IM
import Data.Maybe (fromJust)


import Parser


main :: IO ()
main = do
  inp <- readInput
  putStrLn $ "solve me"


type Input = Program

type Program = IM.IntMap Command

data Command
  = Sound Value
  | Set Register Value
  | Add Register Value
  | Mul Register Value
  | Mod Register Value
  | Receive Value
  | Jump Value Value
  deriving Show


data Value
  = RegValue Register
  | IntValue Int
  deriving Show


type Register = Char


readInput :: IO Input
readInput = parseInput <$> readFile "input.txt"


parseInput :: String -> Input
parseInput = IM.fromList . zip [0..] . map (fromJust . eval commandP) . lines


commandP :: Parser Command
commandP = parseOneOf
  [ unP  Sound   "snd" valueP
  , binP Set     "set" registerP valueP
  , binP Add     "add" registerP valueP
  , binP Mul     "mul" registerP valueP
  , binP Mod     "mod" registerP valueP
  , unP  Receive "rcv" valueP
  , binP Jump    "jgz" valueP valueP
  ]
  where
    unP  c s p     = c <$> (parseString s *> ignoreWhiteSpace *> p <* ignoreWhiteSpace)
    binP c s pa pb = c <$> (parseString s *> ignoreWhiteSpace *> pa) <*> (ignoreWhiteSpace *> pb <* ignoreWhiteSpace)


valueP :: Parser Value
valueP = parseOneOf [ RegValue <$> registerP, IntValue <$> parseInt ]


registerP :: Parser Register
registerP = parsePred isLetter <* ignoreWhiteSpace
