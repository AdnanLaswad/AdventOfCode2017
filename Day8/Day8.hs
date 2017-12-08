{-# LANGUAGE TemplateHaskell #-}
-- Solution to Day 8 of the Advent Of Code 2017

module Main where

import Control.Lens
import Control.Monad (when)
import Control.Monad.State (State, execState)

import Data.Function (on)
import Data.List (maximumBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe, fromMaybe)

import Parser


-- | the program statistics are just the highest
--   register value
newtype Stats =
  Stats
  { _highestVal :: Int
  }

makeLenses ''Stats


type Program = [Command]


data Command =
  Command
  { cmdRegister :: RegName
  , cmdOp       :: Operation
  , cmdCond     :: Condition
  }


type Operation = Int -> Int


type Check = Int -> Bool


type Register = Map RegName Int


type RegName = String


data Condition =
  Condition
  { condRegister :: RegName
  , condCheck    :: Check
  }


type Runtime a = State (Stats, Register) a


----------------------------------------------------------------------
-- MAIN

main :: IO ()
main = do
  inp <- readInput
  let (part1, part2) = solution inp
  putStrLn $ "part1: " ++ show part1
  putStrLn $ "part2: " ++ show part2


solution :: Program -> (Int, Int)
solution inp =
  let (stats, res) = run inp
      regValues = Map.toList res
      highestRegAfter = snd . maximumBy (compare `on` snd) $ regValues
  in (highestRegAfter, view highestVal stats)


----------------------------------------------------------------------
-- interpreter

run :: Program -> (Stats, Register)
run = flip execState (Stats 0, Map.empty) . mapM interpret


interpret :: Command -> Runtime ()
interpret cmd = do
  condMet <- conditionMet $ cmdCond cmd
  when condMet $
    adjustRegister (cmdOp cmd) (cmdRegister cmd)


conditionMet :: Condition -> Runtime Bool
conditionMet cond =
  condCheck cond <$> getRegister (condRegister cond)


adjustRegister :: (Int -> Int) -> RegName -> Runtime ()
adjustRegister f name = do
  v <- getRegister name
  setRegister name (f v)


getRegister :: RegName -> Runtime Int
getRegister name = fromMaybe 0 <$> use (_2.at name)


setRegister :: RegName -> Int -> Runtime ()
setRegister name val = do
  -- update the highest register value in the stats
  _1.highestVal %= max val
  _2.at name .= Just val

----------------------------------------------------------------------
-- parsing

readInput :: IO Program
readInput = parseFile "input.txt"

parseFile :: String -> IO Program
parseFile file = mapMaybe readLine . lines <$> readFile file

readLine :: String -> Maybe Command
readLine = eval cmdP
 
cmdP :: Parser Command
cmdP = Command <$> regNameP <*> operationP <*> condP

operationP :: Parser Operation
operationP = operatorP <*> valueP

condP :: Parser Condition
condP = do
  parseString "if"
  ignoreWhiteSpace
  reg <- regNameP
  chk <- checkP
  return $ Condition reg chk

checkP :: Parser Check
checkP = (flip <$> comparisionP) <*> valueP

regNameP :: Parser RegName
regNameP = parseAlphas <* ignoreWhiteSpace

valueP :: Parser Int
valueP = parseInt <* ignoreWhiteSpace

comparisionP :: Parser (Int -> Int -> Bool)
comparisionP = parseOneOf
  [ parseString "<=" *> pure (<=)
  , parseString "<" *> pure (<)
  , parseString "!=" *> pure (/=)
  , parseString "==" *> pure (==)
  , parseString ">=" *> pure (>=)
  , parseString ">" *> pure (>)
  ]

operatorP :: Parser (Int -> Int -> Int)
operatorP = parseOneOf
  [ parseString "inc" *> pure (+)
  , parseString "dec" *> pure subtract
  ] <* ignoreWhiteSpace

