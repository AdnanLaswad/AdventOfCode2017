-- Solution to Day 17 of the Advent Of Code 2017
module Main where

import Data.Maybe (fromJust)
import Data.List (foldl')
import qualified Data.Sequence as S

main :: IO ()
main = do
  inp <- readInput

  putStrLn $ "part 1: " ++ show (part1 inp)


type Input = Int

data CircularBuffer =
  CB { bufferContent :: S.Seq Int
     , bufferIndex   :: Int
     , bufferLength  :: Int
     } deriving Show


part1 :: Input -> Int
part1 inp = getAfter 2017 $ run inp 2017


getAfter :: Int -> CircularBuffer -> Int
getAfter val cb =
  let ind  = fromJust $ S.elemIndexL val (bufferContent cb)
      ind' = ind + 1 `mod` bufferLength cb
  in S.index (bufferContent cb) ind'


run :: Int -> Int -> CircularBuffer
run key cnt =
  foldl' (flip (step key)) initBuffer [1..cnt]


step :: Int -> Int -> CircularBuffer -> CircularBuffer
step key val cb =
  let cb' = stepBuffer key cb
  in insertValue val cb'


initBuffer :: CircularBuffer
initBuffer = CB (S.singleton 0) 0 1


stepBuffer :: Int -> CircularBuffer -> CircularBuffer
stepBuffer n cb = cb { bufferIndex = (bufferIndex cb + n) `mod` bufferLength cb }


insertValue :: Int -> CircularBuffer -> CircularBuffer
insertValue val cb = cb { bufferContent = insertAt (bufferIndex cb) val (bufferContent cb)
                        , bufferIndex   = bufferIndex cb + 1
                        , bufferLength  = bufferLength cb + 1
                        }


insertAt :: Int -> a -> S.Seq a -> S.Seq a
insertAt ind val ss =
  let (sa, sb) = S.splitAt (ind + 1) ss
  in (sa S.|> val) S.>< sb


readInput :: IO Input
readInput = return 304
