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

  let part1 = run 0 prg
  putStrLn $ "part1 : " ++ show part1

  let part2 = runPar prg
  putStrLn $ "part2 : " ++ show part2


type Input = Program

type Program = IM.IntMap Command

data Command
  = Sound Value
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
  { registers :: M.Map Char Integer
  , programm  :: Program
  , pointer   :: Int
  , sound     :: Integer
  } deriving Show


type Runtime a = ST.State Environment a


type RuntimePar a = ST.State EnvironmentPar a


data EnvironmentPar =
  EnvPar
  { parProgramm   :: Program
  , parRegisters0 :: M.Map Char Integer
  , parRegisters1 :: M.Map Char Integer
  , parPointer0   :: ParPointer
  , parPointer1   :: ParPointer
  , recQueue0     :: [Integer]
  , recQueue1     :: [Integer]
  , send1         :: Int
  } deriving Show


data ParPointer
  = Receiving ProgramPointer
  | Next      ProgramPointer
  deriving Show


type ProgramPointer = Int


runPar :: Program -> Int
runPar prg =
  let env = ST.execState loopPar (initEnvironmentPar prg)
  in send1 env


loopPar :: RuntimePar ()
loopPar = do
  env <- ST.get
  pt0 <- getPointerPar 0
  pt1 <- getPointerPar 1
  case (pt0, pt1) of
    (Receiving _, Receiving _) ->
      return ()
    (Next i0, Receiving _) -> do
      _ <- interpretPar 0 i0
      loopPar
    (Receiving _, Next i1) -> do
      _ <- interpretPar 1 i1
      loopPar
    (Next i0, Next i1) -> do
      _ <- interpretPar 0 i0
      _ <- interpretPar 1 i1
      loopPar


interpretPar :: Int -> ProgramPointer -> RuntimePar ()
interpretPar prgNr pt = do
  cmd <- getCommandPar pt
  case cmd of
    Sound v -> do
      f <- getValuePar prgNr v
      send (1-prgNr) f
      if prgNr == 1
        then ST.modify (\e -> e { send1 = send1 e + 1})
        else return ()
      nextPar prgNr
    Set r v -> do
      f <- getValuePar prgNr v
      setRegisterPar prgNr r f
      nextPar prgNr
    Add r v -> do
      b <- getValuePar prgNr v
      a <- getRegisterPar prgNr r
      setRegisterPar prgNr r (a+b)
      nextPar prgNr
    Mul r v -> do
      b <- getValuePar prgNr v
      a <- getRegisterPar prgNr r
      setRegisterPar prgNr r (a*b)
      nextPar prgNr
    Mod r v -> do
      b <- getValuePar prgNr v
      a <- getRegisterPar prgNr r
      setRegisterPar prgNr r (a `mod` b)
      nextPar prgNr
    Receive r -> do
      recv <- receivePar prgNr
      case recv of
        Nothing -> waitPar prgNr
        Just v  -> do
          setRegisterPar prgNr r v
          nextPar prgNr
    Jump bv ov -> do
      b <- getValuePar prgNr bv
      if b > 0
      then do
        o <- getValuePar prgNr ov
        jumpPar prgNr (fromInteger o)
      else
        nextPar prgNr


nextPar :: Int -> RuntimePar ()
nextPar prgNr = do
  pt <- getPointerPar prgNr
  let pt' =
        case pt of
          Receiving i -> error "next from waiting" -- Next $ i+1
          Next      i -> Next $ i+1
  setPointerPar prgNr pt'


jumpPar :: Int -> Int -> RuntimePar ()
jumpPar prgNr delta = do
  pt <- getPointerPar prgNr
  let pt' =
        case pt of
          Receiving i -> error "can't jump while waiting"
          Next      i -> Next $ i+delta
  setPointerPar prgNr pt'


waitPar :: Int -> RuntimePar ()
waitPar prgNr = do
  pt <- getPointerPar prgNr
  let pt' =
        case pt of
          Receiving _ -> error "can't wait while waiting"
          Next      i -> Receiving i
  setPointerPar prgNr pt'


getPointerPar :: Int -> RuntimePar ParPointer
getPointerPar prgNr = do
  pt <- ST.gets (\e -> if prgNr == 0 then parPointer0 e else parPointer1 e)
  case pt of
    Next _      -> return pt
    Receiving i -> do
      found <- not <$> ST.gets (\e -> if prgNr == 0 then null (recQueue0 e) else null (recQueue1 e))
      if found
        then do
          ST.modify (\e -> if prgNr == 0 then e { parPointer0 = Next i} else e { parPointer1 = Next i})
          return (Next i)
        else return pt


setPointerPar :: Int -> ParPointer -> RuntimePar ()
setPointerPar prgNr pt = 
  ST.modify (\e -> if prgNr == 0 then e { parPointer0 = pt } else e { parPointer1 = pt })


send :: Int -> Integer -> RuntimePar ()
send prgNr sig =
  ST.modify (\e -> if prgNr == 0 then e { recQueue0 = recQueue0 e ++ [sig] } else e { recQueue1 = recQueue1 e ++ [sig] })


receivePar :: Int -> RuntimePar (Maybe Integer)
receivePar prgNr = do
  qu <- ST.gets (\e -> if prgNr == 0 then recQueue0 e else recQueue1 e)
  if null qu then return Nothing else do
    let qu' = tail qu
        sig = head qu
    ST.modify (\e -> if prgNr == 0 then e { recQueue0 = qu' } else e { recQueue1 = qu' })
    return $ Just sig


run :: Int -> Program -> Integer
run prgNr prg = ST.evalState interpret (initEnvironment prgNr prg)


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
    Receive r -> do
      f <- getRegister r
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


initEnvironment :: Int -> Program -> Environment
initEnvironment prgNr prg = Environment (M.fromList [('p', fromIntegral prgNr)]) prg 0 0


initEnvironmentPar :: Program -> EnvironmentPar
initEnvironmentPar prg = EnvPar
  prg
  (M.fromList [('p', fromIntegral 0)])
  (M.fromList [('p', fromIntegral 1)])
  (Next 0)
  (Next 0)
  []
  []
  0


getCommand :: Runtime Command
getCommand = ST.gets (\e -> programm e IM.! pointer e)


getCommandPar :: ProgramPointer -> RuntimePar Command
getCommandPar pt = ST.gets (\e -> parProgramm e IM.! pt)


getValue :: Value -> Runtime Integer
getValue (RegValue r) = getRegister r
getValue (IntValue i) = return (fromIntegral i)


getValuePar :: Int -> Value -> RuntimePar Integer
getValuePar prgNr (RegValue r) = getRegisterPar prgNr r
getValuePar _     (IntValue i) = return (fromIntegral i)


getRegister :: Register -> Runtime Integer
getRegister r = ST.gets (\e -> fromMaybe 0 $ M.lookup r $ registers e)


getRegisterPar :: Int -> Register -> RuntimePar Integer
getRegisterPar prgNr r = ST.gets (\e -> fromMaybe 0 $ M.lookup r $ if prgNr == 0 then parRegisters0 e else parRegisters1 e)


movePointer :: Int -> Runtime ()
movePointer delta = ST.modify (\e -> e { pointer = pointer e + delta })


setRegister :: Register -> Integer -> Runtime ()
setRegister r v = ST.modify (\e -> e { registers = M.insert r v $ registers e })


setRegisterPar :: Int -> Register -> Integer -> RuntimePar ()
setRegisterPar prgNr r v = ST.modify (\e -> if prgNr == 0
                                            then e { parRegisters0 = M.insert r v $ parRegisters0 e }
                                            else e { parRegisters1 = M.insert r v $ parRegisters1 e })


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
