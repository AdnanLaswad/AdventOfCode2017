{-# LANGUAGE ScopedTypeVariables #-}
-- Solution to Day 23 of the Advent Of Code 2017

module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (BlockedIndefinitelyOnSTM, catch)
import Control.Monad (void)
import Control.Monad.Reader as R
import Data.Char (isLetter)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, fromMaybe)
import Parser
import Debug.Trace


main :: IO ()
main = do
  prg <- readInput
  part1 <- run prg

  putStrLn $ "part2 : " ++ show part1


type Input = Program

type Program = IM.IntMap Command

data Command
  = Set Register Value
  | Sub Register Value
  | Mul Register Value
  | JumpNotZero Value Value
  deriving Show


data Value
  = RegValue Register
  | IntValue Int
  deriving Show


type Register = Char


data Environment =
  Environment
  { registers   :: TVar (M.Map Char Signal)
  , program     :: Program
  , pointer     :: TVar ProgramPointer
  , mulCounter  :: TVar Int
  }


type ProgramPointer = Int

type Signal = Integer

type Runtime a = R.ReaderT Environment IO a


run :: Program -> IO Int
run prg = do
  env <- initEnvironment prg
  R.runReaderT interpret env


interpret :: Runtime Int
interpret = do
  cmd <- getCommand
  case cmd of
    Nothing ->
      getMulCounter

    Just (Set r v) -> do
      f <- getValue v
      setRegister r f
      next
    Just (Sub r v) -> do
      b <- getValue v
      a <- getRegister r
      setRegister r (a-b)
      next
    Just (Mul r v) -> do
      b <- getValue v
      a <- getRegister r
      setRegister r (a*b)
      incrMulCounter
      next
    Just (JumpNotZero bv ov) -> do
      b <- getValue bv
      if b /= 0
      then do
        o <- getValue ov
        movePointer (fromInteger o)
        interpret
      else
        next


next :: Runtime Int
next = movePointer 1 >> interpret


initEnvironment :: Program -> IO Environment
initEnvironment prg = do
  env <- create
  return env
  where
    create = do
      ptr  <- newTVarIO 0
      regs <- newTVarIO M.empty
      mulCnt <- newTVarIO 0
      return $ Environment regs prg ptr mulCnt


getMulCounter :: Runtime Int
getMulCounter = R.asks mulCounter >>= lift . atomically . readTVar


incrMulCounter :: Runtime ()
incrMulCounter = R.asks mulCounter >>= lift . atomically . ( `modifyTVar'` (+ 1)) 


getCommand :: Runtime (Maybe Command)
getCommand = do
  prg <- R.asks program
  pt  <- R.asks pointer >>= lift . atomically . readTVar
  let cmd = IM.lookup pt prg
  return cmd


movePointer :: Int -> Runtime ()
movePointer delta =
  R.asks pointer >>= lift . atomically . (`modifyTVar'` (+ delta))


getValue :: Value -> Runtime Integer
getValue (RegValue r) = getRegister r
getValue (IntValue i) = return (fromIntegral i)


getRegister :: Register -> Runtime Integer
getRegister r = do
  regs <- R.asks registers >>= lift . atomically . readTVar
  return $ fromMaybe 0 $ M.lookup r regs


setRegister :: Register -> Integer -> Runtime ()
setRegister r v =
  R.asks registers >>= lift . atomically . (`modifyTVar'` M.insert r v)


----------------------------------------------------------------------
-- input / parsing

readInput :: IO Input
readInput = parseInput <$> readFile "input.txt"


parseInput :: String -> Input
parseInput = IM.fromList . zip [0..] . map (fromJust . eval commandP) . lines


commandP :: Parser Command
commandP = parseOneOf
  [ binP Set         "set" registerP valueP
  , binP Sub         "sub" registerP valueP
  , binP Mul         "mul" registerP valueP
  , binP JumpNotZero "jnz" valueP valueP
  ]
  where
    unP  c s p     = c <$> (parseString s *> ignoreWhiteSpace *> p <* ignoreWhiteSpace)
    binP c s pa pb = c <$> (parseString s *> ignoreWhiteSpace *> pa) <*> (ignoreWhiteSpace *> pb <* ignoreWhiteSpace)


valueP :: Parser Value
valueP = parseOneOf [ RegValue <$> registerP, IntValue <$> parseInt ]


registerP :: Parser Register
registerP = parsePred isLetter <* ignoreWhiteSpace
