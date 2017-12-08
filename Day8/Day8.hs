-- Solution to Day 8 of the Advent Of Code 2017

module Main where

import Data.Function (on)
import Data.List (foldl', sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe, fromMaybe, fromJust)

import Parser



main :: IO ()
main = do
  inp <- readInput
  let (part1, part2) = solution inp
  putStrLn $ "part1: " ++ show part1
  putStrLn $ "part2: " ++ show part2


solution :: Input -> (Int, Int)
solution inp =
  let (highest, res) = run inp
      regValues = Map.toList res
      highestRegAfter = snd . last . sortBy (compare `on` snd) $ regValues
  in (highestRegAfter, highest)


----------------------------------------------------------------------
-- interpreter

run :: Input -> (Int, Register)
run = foldl' interpret (minBound, Map.empty)


interpret :: (Int, Register) -> Command -> (Int, Register)
interpret (highest, rs) cmd =
  if conditionMet (cmdCond cmd) then
    let rs' = Map.alter (Just . op . fromMaybe 0) (cmdRegister cmd) rs
        v   = fromJust $ Map.lookup (cmdRegister cmd) rs'
    in (max v highest, rs')
  else
    (highest, rs)
  where
    op =
      case cmdOp cmd of
        Incr -> (+ (cmdValue cmd))
        Decr -> subtract (cmdValue cmd)
    conditionMet cond =
      let rVal = fromMaybe 0 $ Map.lookup (condRegister cond) rs
          cVal = condValue cond
      in case condOp cond of
        Less      -> rVal <  cVal
        LessEq    -> rVal <= cVal
        Equal     -> rVal == cVal
        NotEqual  -> rVal /= cVal
        Greater   -> rVal >  cVal
        GreaterEq -> rVal >= cVal


----------------------------------------------------------------------
-- data and parsing

data Command =
  Command
  { cmdRegister :: RegName
  , cmdOp       :: Operation
  , cmdValue    :: Int
  , cmdCond     :: Condition
  } deriving Show

data Operation
  = Incr | Decr
  deriving Show


data Operator
  = Less | LessEq | Equal | NotEqual | Greater | GreaterEq
  deriving Show

type Register = Map RegName Int

type RegName = String

data Condition =
  Condition
  { condRegister :: RegName
  , condOp       :: Operator
  , condValue    :: Int
  } deriving Show


type Input = [Command]

readInput :: IO Input
readInput = parseFile "input.txt"

parseFile :: String -> IO Input
parseFile file = mapMaybe readLine . lines <$> readFile file

readLine :: String -> Maybe Command
readLine = eval cmdP

cmdP :: Parser Command
cmdP = Command <$> regNameP <*> operationP <*> valueP <*> condP

regNameP :: Parser RegName
regNameP = parseAlphas <* ignoreWhiteSpace

operationP :: Parser Operation
operationP = parseEither (parseString "inc" *> pure Incr) (parseString "dec" *> pure Decr) <* ignoreWhiteSpace

valueP :: Parser Int
valueP = parseInt <* ignoreWhiteSpace

condP :: Parser Condition
condP = do
  parseString "if"
  ignoreWhiteSpace
  reg <- regNameP
  op  <- operatorP
  v   <- valueP
  return $ Condition reg op v


operatorP :: Parser Operator
operatorP =
  parseEither
  ( parseEither
    (parseString "<=" *> pure LessEq)
    (parseString "<" *> pure Less)
  )
  (parseEither ( parseEither
                 (parseString "!=" *> pure NotEqual)
                 (parseString "==" *> pure Equal)
               )
               ( parseEither
                 (parseString ">=" *> pure GreaterEq)
                 (parseString ">" *> pure Greater)
               )
  )

exampleLine :: String
exampleLine = "g dec 231 if bfx > -10"
