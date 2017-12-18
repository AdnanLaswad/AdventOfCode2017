{-# LANGUAGE ScopedTypeVariables #-}
-- Solution to Day 18 of the Advent Of Code 2017

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


main :: IO ()
main = do
  prg <- readInput
  part2 <- run prg
  putStrLn $ "part2 : " ++ show part2


type Input = Program

type Program = IM.IntMap Command

data Command
  = Send Value
  | Set Register Value
  | Add Register Value
  | Mul Register Value
  | Mod Register Value
  | Receive Register
  | Jump Value Value
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
  , receive     :: STM Signal
  , send        :: Integer -> STM ()
  , sendCounter :: TVar Int
  }


type ProgramPointer = Int

type Signal = Integer

type Runtime a = R.ReaderT Environment IO a


run :: Program -> IO Int
run prg = do
  (env0, env1) <- initEnvironments prg
  _ <- forkIO . void $ R.runReaderT (interpret 0) env0
  R.runReaderT (interpret 1) env1


interpret :: Int -> Runtime Int
interpret prgNr = do
  cmd <- getCommand
  case cmd of
    Send v -> do
      f <- getValue v
      sendSignal f
      next prgNr
    Set r v -> do
      f <- getValue v
      setRegister r f
      next prgNr
    Add r v -> do
      b <- getValue v
      a <- getRegister r
      setRegister r (a+b)
      next prgNr
    Mul r v -> do
      b <- getValue v
      a <- getRegister r
      setRegister r (a*b)
      next prgNr
    Mod r v -> do
      b <- getValue v
      a <- getRegister r
      setRegister r (a `mod` b)
      next prgNr
    Receive r -> do
      sig <- receiveSignal
      case sig of
        Nothing ->
          getSendCounter
        Just val -> do
          setRegister r val
          next prgNr
    Jump bv ov -> do
      b <- getValue bv
      if b > 0
      then do
        o <- getValue ov
        movePointer (fromInteger o)
        interpret prgNr
      else
        next prgNr


next :: Int -> Runtime Int
next prgNr = movePointer 1 >> interpret prgNr


initEnvironments :: Program -> IO (Environment, Environment)
initEnvironments prg = do
  qu0 <- newTQueueIO
  qu1 <- newTQueueIO
  env0 <- create 0 qu0 qu1
  env1 <- create 1 qu1 qu0
  return (env0, env1)
  where
    sendTo sc queueOther sig = do
      modifyTVar' sc (+ 1)
      writeTQueue queueOther sig
    create prgNr queue queueOther = do
      ptr  <- newTVarIO 0
      regs <- newTVarIO (M.fromList [('p', prgNr)])
      scnt <- newTVarIO 0
      return $ Environment regs prg ptr (readTQueue queue) (sendTo scnt queueOther) scnt


sendSignal :: Signal -> Runtime ()
sendSignal sig =
  R.asks send >>= lift . atomically . ($ sig)


receiveSignal :: Runtime (Maybe Signal)
receiveSignal =
  R.asks receive >>= \rcv -> lift $ atomically (Just <$> rcv) `catch` onDeadlock
  where
    onDeadlock :: BlockedIndefinitelyOnSTM -> IO (Maybe Signal)
    onDeadlock _ = return Nothing


getSendCounter :: Runtime Int
getSendCounter = R.asks sendCounter >>= lift . atomically . readTVar


getCommand :: Runtime Command
getCommand = do
  prg <- R.asks program
  pt  <- R.asks pointer >>= lift . atomically . readTVar
  return $ prg IM.! pt


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
  [ unP  Send    "snd" valueP
  , binP Set     "set" registerP valueP
  , binP Add     "add" registerP valueP
  , binP Mul     "mul" registerP valueP
  , binP Mod     "mod" registerP valueP
  , unP  Receive "rcv" registerP
  , binP Jump    "jgz" valueP valueP
  ]
  where
    unP  c s p     = c <$> (parseString s *> ignoreWhiteSpace *> p <* ignoreWhiteSpace)
    binP c s pa pb = c <$> (parseString s *> ignoreWhiteSpace *> pa) <*> (ignoreWhiteSpace *> pb <* ignoreWhiteSpace)


valueP :: Parser Value
valueP = parseOneOf [ RegValue <$> registerP, IntValue <$> parseInt ]


registerP :: Parser Register
registerP = parsePred isLetter <* ignoreWhiteSpace
