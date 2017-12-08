-- Solution to Day 8 of the Advent Of Code 2017

module Main where

import Control.Monad (foldM)
import Control.Monad.State (State)
import qualified Control.Monad.State as St

import Data.Function (on)
import Data.List (maximumBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe, fromMaybe)

import Parser



main :: IO ()
main = do
  inp <- readInput
  let (part1, part2) = solution inp
  putStrLn $ "part1: " ++ show part1
  putStrLn $ "part2: " ++ show part2


solution :: Program -> (Int, Int)
solution inp =
  let (highest, res) = run inp
      regValues = Map.toList res
      highestRegAfter = snd . maximumBy (compare `on` snd) $ regValues
  in (highestRegAfter, highest)


----------------------------------------------------------------------
-- interpreter

-- | the program statistics are just the highest
--   register value
type Stats = Int


run :: Program -> (Stats, Register)
run = flip St.runState Map.empty . foldM interpret minBound


interpret :: Stats -> Command -> Runtime Stats
interpret highest cmd = do
  condMet <- conditionMet $ cmdCond cmd
  if condMet then do
    v <- adjustRegister (cmdOp cmd) (cmdRegister cmd)
    return $ max v highest
  else
    return highest


conditionMet :: Condition -> Runtime Bool
conditionMet cond =
  condCheck cond <$> getRegister (condRegister cond)


type Runtime a = State Register a


adjustRegister :: (Int -> Int) -> RegName -> Runtime Int
adjustRegister f name = do
  val <- getRegister name
  let val' = f val
  setRegister name val'
  return val'


getRegister :: RegName -> Runtime Int
getRegister name = fromMaybe 0 <$> St.gets (Map.lookup name)


setRegister :: RegName -> Int -> Runtime ()
setRegister name val = St.modify (Map.insert name val)

----------------------------------------------------------------------
-- data and parsing

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


type Program = [Command]

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

