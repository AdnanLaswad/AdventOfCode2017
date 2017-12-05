-- Solution to Day 5 of the Advent Of Code 2017

module Main where


import Prelude hiding (init)
import Data.Maybe (isNothing)


main :: IO ()
main = do
  inp <- readInput
  maschine <- init <$> readInput

  let steps1 = process incr maschine
  putStrLn $ "part 1: machine took " ++ show steps1 ++ " steps"

  let steps2 = process strange maschine
  putStrLn $ "part 2: machine took " ++ show steps2 ++ " steps"


example :: Zipper Jump
example = init $ map Jump [0,3,0,1,(-3)]


----------------------------------------------------------------------
-- Input

type Input = [Jump]

readInput :: IO Input
readInput = map (Jump . read). lines <$> readFile "input.txt"


----------------------------------------------------------------------
-- algorithm

data Jump = Jump Int
  deriving Show


process :: OffsetJump -> Zipper Jump -> Int
process upd z =
  if isStopped z then 0 else 1 + process upd (step upd z)


step :: OffsetJump -> Zipper Jump -> Zipper Jump
step upd z =
  case current z of
    Nothing       -> z
    Just (Jump n) ->
      let z' = update (upd n) z
          f  = if n >= 0 then moveRight else moveLeft
      in ntimes f (abs n) z'


isStopped :: Zipper Jump -> Bool
isStopped = isNothing . current


init :: Input -> Zipper Jump
init jmps = Zipper [] jmps


type OffsetJump = Int -> Jump -> Jump


incr :: OffsetJump
incr _ (Jump n) = Jump (n+1)


strange :: OffsetJump
strange n (Jump j)
  | n >= 3    = Jump (j-1)
  | otherwise = Jump (j+1)


----------------------------------------------------------------------
-- Zipper

data Zipper a =
  Zipper { left :: [a]
         , right :: [a]
         }
  deriving Show


update :: (a -> a) -> Zipper a -> Zipper a
update _ z@(Zipper _ []) = z
update f (Zipper ls (c:rs)) = Zipper ls (f c : rs)


current :: Zipper a -> Maybe a
current (Zipper _ []) = Nothing
current (Zipper _ (c:_)) = Just c


moveRight :: Zipper a -> Zipper a
moveRight z@(Zipper _ []) = z
moveRight (Zipper ls (c:rs)) = Zipper (c:ls) rs


moveLeft :: Zipper a -> Zipper a
moveLeft z@(Zipper [] _) = z
moveLeft (Zipper (c:ls) rs) = Zipper ls (c:rs)


----------------------------------------------------------------------
-- helper

ntimes :: (a -> a) -> Int -> a -> a
ntimes _ 0 a = a
ntimes f n a = ntimes f (n-1) (f a)


