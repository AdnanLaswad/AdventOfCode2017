-- Solution to Day 18 of the Advent Of Code 2017

module Main where

import Control.Monad.State as ST

import Data.Char (isLetter)
import qualified Data.IntMap.Strict as IM
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Map.Strict as M
import Debug.Trace

import Parser


main :: IO ()
main = do
  prg <- readInput

  let part1 = run prg
  putStrLn $ "part1 : " ++ show part1


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


data Environment =
  Environment
  { registers :: M.Map Char Integer
  , programm  :: Program
  , pointer   :: Int
  , sound     :: Integer
  } deriving Show


type Runtime a = ST.State Environment a


run :: Program -> Integer
run prg = ST.evalState interpret (initEnvironment prg)


interpret :: Runtime Integer
interpret = do
  cmd <- getCommand
  case cmd of
    Sound v -> do
      f <- getValue v
      setSound f
      next
    Set r v -> do
      f <- getValue v
      setRegister r f
      next
    Add r v -> do
      b <- getValue v
      a <- getRegister r
      setRegister r (a+b)
      next
    Mul r v -> do
      b <- getValue v
      a <- getRegister r
      setRegister r (a*b)
      next
    Mod r v -> do
      b <- getValue v
      a <- getRegister r
      setRegister r (a `mod` b)
      next
    Receive v -> do
      f <- getValue v
      if f /= 0 then getSound else next
    Jump bv ov -> do
      b <- getValue bv
      if b > 0
      then do
        o <- getValue ov
        movePointer (fromInteger o)
        interpret
      else
        next


next :: Runtime Integer
next = movePointer 1 >> interpret


initEnvironment :: Program -> Environment
initEnvironment prg = Environment M.empty prg 0 0


getCommand :: Runtime Command
getCommand = ST.gets (\e -> programm e IM.! pointer e)


getValue :: Value -> Runtime Integer
getValue (RegValue r) = getRegister r
getValue (IntValue i) = return (fromIntegral i)


getRegister :: Register -> Runtime Integer
getRegister r = ST.gets (\e -> fromMaybe 0 $ M.lookup r $ registers e)


movePointer :: Int -> Runtime ()
movePointer delta = ST.modify (\e -> e { pointer = pointer e + delta })


setRegister :: Register -> Integer -> Runtime ()
setRegister r v = ST.modify (\e -> e { registers = M.insert r v $ registers e })


getSound :: Runtime Integer
getSound = ST.gets sound


setSound :: Integer -> Runtime ()
setSound f = ST.modify (\e -> e { sound = f })


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
