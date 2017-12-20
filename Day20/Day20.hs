-- Solution to Day 20 of the Advent Of Code 2017

module Main where

import Data.Function (on)
import Data.List (maximumBy, sortBy, groupBy, (\\))
import Data.Maybe (fromJust, maybeToList)
import qualified Data.Set as S
import Parser
import Debug.Trace

main :: IO ()
main = do
  inp <- readInput

  putStrLn $ "part 1: " ++ show (number $ minimum inp)

  -- simulate says 420!
  -- let ps' = simulate inp
  -- print ps'

  -- but this says 423 - SHIIT
  let part2 = length $ removeCollisions inp
  putStrLn $ "part 2: " ++ show part2


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


tick :: Particle -> Particle
tick (Particle n p v a) = (Particle n p' v' a)
  where v' = add v a
        p' = add p v'
        add (x,y,z) (x',y',z') = (x+x',y+y',z+z')


areColliding :: [Particle] -> [Particle]
areColliding ps = concat [[a,b] | (a,b) <- pickTwo ps, position a == position b ]


simulate :: [Particle] -> [Particle]
simulate ps =
  let ps' = ps \\ areColliding ps
  in trace (show $ length ps') $ simulate (tick <$> ps')


collide :: Particle -> Particle -> Maybe Int
collide pa pb = go 0 (dist (position pa) (position pb)) pa pb
  where go t oldD px py =
          let px' = tick px
              py' = tick py
              d   = dist (position px) (position py)
          in if d == 0
             then Just t
             else if oldD < d then Nothing else go (t+1) d px' py'


collisions :: [Particle] -> [(Particle,Particle)]
collisions ps =
  let grps = groupBy ((==) `on` \(t,_,_) -> t) $ sortBy (compare `on` \(t,_,_) -> t)
             [ (t,pa,pb) | (pa,pb) <- pickTwo ps
                         , t <- maybeToList (collide pa pb) ]
  in if null grps then [] else map (\(_,a,b) -> (a,b)) $ head grps


removeCollisions :: [Particle] -> [Particle]
removeCollisions ps =
  let cols = map number $ concatMap (\(a,b) -> [a,b]) $ collisions ps
  in if null cols
     then ps
     else let ps' = filter (\p -> not (number p `elem` cols)) ps
          in removeCollisions ps'


pickTwo :: [a] -> [(a,a)]
pickTwo as = do
  (a,as') <- pick as
  (b,_)   <- pick as'
  return (a,b)


pick :: [a] -> [(a,[a])]
pick [] = []
pick (a:as) = (a,as) : pick as


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

