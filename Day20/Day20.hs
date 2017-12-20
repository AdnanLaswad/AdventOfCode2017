-- Solution to Day 20 of the Advent Of Code 2017

module Main where

import Data.Function (on)
import Data.List (maximumBy)
import Data.Maybe (fromJust)
import Parser

main :: IO ()
main = do
  inp <- readInput
  putStrLn $ "solve me"


type Input = [Particle]


type Position = (Int,Int,Int)
type Velocity = (Int,Int,Int)
type Acceleration = (Int,Int,Int)


data Particle =
  Particle
  { number       :: Int
  , position     :: Position
  , velocity     :: Velocity
  , acceleration :: Acceleration
  } deriving (Show)


instance Eq Particle where
  pa == pb = pa `compare` pb == EQ


instance Ord Particle where
  (Particle _ pa va aa) `compare` (Particle _ pb vb ab) =
    case norm aa `compare` norm ab of
      EQ -> case norm va `compare` norm vb of
              EQ -> norm pa `compare` norm pb
              o  -> o
      o  -> o


dist :: Num a => (a,a,a) -> (a,a,a) -> a
dist (x,y,z) (x',y',z') = abs (x-x') + abs (y-y') + abs (z-z')


norm :: Num a => (a,a,a) -> a
norm = dist (0,0,0)


readInput :: IO Input
readInput = zipWith (\i -> fromJust . eval (particleP i)) [0..]  . lines <$> readFile "input.txt"


particleP :: Int -> Parser Particle
particleP nr = Particle nr <$> posP <* parseString ", " <*> velP <* parseString ", " <*> accP


posP :: Parser Position
posP =
  (,,) <$> (parseString "p=<" *> parseInt <* parseString ",")
       <*> (parseInt <* parseString ",")
       <*> (parseInt <* parseString ">")


velP :: Parser Velocity
velP =
  (,,) <$> (parseString "v=<" *> parseInt <* parseString ",")
       <*> (parseInt <* parseString ",")
       <*> (parseInt <* parseString ">")


accP :: Parser Acceleration
accP =
  (,,) <$> (parseString "a=<" *> parseInt <* parseString ",")
       <*> (parseInt <* parseString ",")
       <*> (parseInt <* parseString ">")

