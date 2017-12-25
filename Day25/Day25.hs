-- Solution to Day 25 of the Advent Of Code 2017

module Main where

import qualified Data.IntSet as IS
import Control.Monad.State.Strict


main :: IO ()
main = do
  putStrLn $ "part 1: " ++ show part1


type Computation a = State Env a

data Env =
  Env
  { tape     :: Tape
  , position :: Position
  } deriving Show

type Tape          = IS.IntSet
type Position      = Int


data TuringState = A | B | C | D | E | F
  deriving Show


part1 :: Int
part1 = diagnosticChecksum $ execState (run puzzleSteps A) (Env IS.empty 0)


puzzleSteps :: Int
puzzleSteps = 12173597


diagnosticChecksum :: Env -> Int
diagnosticChecksum env = IS.size $ tape env


run :: Int -> TuringState -> Computation ()
run 0 _ = return ()
run n s = step s >>= run (n-1)


step :: TuringState -> Computation TuringState
step state = do
  cur <- readValue
  case (state, cur) of
    (A, False) -> do
      writeValue True
      moveRight
      return B
    (A, True) -> do
      writeValue False
      moveLeft
      return C

    (B, False) -> do
      writeValue True
      moveLeft
      return A
    (B, True) -> do
      writeValue True
      moveRight
      return D

    (C, False) -> do
      writeValue True
      moveRight
      return A
    (C, True) -> do
      writeValue False
      moveLeft
      return E

    (D, False) -> do
      writeValue True
      moveRight
      return A
    (D, True) -> do
      writeValue False
      moveRight
      return B

    (E, False) -> do
      writeValue True
      moveLeft
      return F
    (E, True) -> do
      writeValue True
      moveLeft
      return C

    (F, False) -> do
      writeValue True
      moveRight
      return D
    (F, True) -> do
      writeValue True
      moveRight
      return A


writeValue :: Bool -> Computation ()
writeValue False = modify' (\env -> env { tape = IS.delete (position env) (tape env) } )
writeValue True  = modify' (\env -> env { tape = IS.insert (position env) (tape env) } )


readValue :: Computation Bool
readValue = gets (\env -> position env `IS.member` tape env)


moveLeft :: Computation ()
moveLeft = move (+ 1)


moveRight :: Computation ()
moveRight = move (subtract 1)


move :: (Int -> Int) -> Computation ()
move upd = modify' (\env -> env { position = upd (position env) })


nTimes :: Int -> (a -> a) -> a -> a
nTimes 0 _ x = x
nTimes n f x = nTimes (n-1) f (f x)
