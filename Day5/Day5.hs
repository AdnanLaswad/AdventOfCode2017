-- Solution to Day 5 of the Advent Of Code 2017

module Main where


import Prelude hiding (init)
import Data.Maybe (isNothing)


main :: IO ()
main = do
  inp <- readInput
  maschine <- init <$> readInput
  let steps = process maschine
  putStrLn $ "part 1: machine took " ++ show steps ++ " steps"


type Input = [Jump]


data Jump = Jump Int
  deriving Show


data Zipper a =
  Zipper { left :: [a]
         , right :: [a]
         }
  deriving Show


init :: Input -> Zipper Jump
init jmps = Zipper [] jmps


process :: Zipper Jump -> Int
process z =
  if isStopped z then 0 else 1 + process (step z)


step :: Zipper Jump -> Zipper Jump
step z =
  case current z of
    Nothing       -> z
    Just (Jump n) ->
      let z' = update incr z
          f  = if n >= 0 then moveRight else moveLeft
      in ntimes f (abs n) z'


ntimes :: (a -> a) -> Int -> a -> a
ntimes _ 0 a = a
ntimes f n a = ntimes f (n-1) (f a)


incr :: Jump -> Jump
incr (Jump n) = Jump (n+1)


update :: (a -> a) -> Zipper a -> Zipper a
update _ z@(Zipper _ []) = z
update f (Zipper ls (c:rs)) = Zipper ls (f c : rs)


isStopped :: Zipper a -> Bool
isStopped = isNothing . current


current :: Zipper a -> Maybe a
current (Zipper _ []) = Nothing
current (Zipper _ (c:_)) = Just c


moveRight :: Zipper a -> Zipper a
moveRight z@(Zipper _ []) = z
moveRight (Zipper ls (c:rs)) = Zipper (c:ls) rs


moveLeft :: Zipper a -> Zipper a
moveLeft z@(Zipper [] _) = z
moveLeft (Zipper (c:ls) rs) = Zipper ls (c:rs)


readInput :: IO Input
readInput = map (Jump . read). lines <$> readFile "input.txt"


example :: Zipper Jump
example = init $ map Jump [0,3,0,1,(-3)]
