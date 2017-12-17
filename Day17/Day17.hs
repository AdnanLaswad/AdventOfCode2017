-- Solution to Day 17 of the Advent Of Code 2017
{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Maybe (fromJust)
import Data.List (foldl')
import qualified Data.Sequence as S

inputKey :: Int
inputKey = 304

main :: IO ()
main = do
  putStrLn $ "part 1: " ++ show (part1 inputKey)
  putStrLn $ "part 2: " ++ show (part2 inputKey)


----------------------------------------------------------------------
-- part 2

-- | don't need to know anything but (length of buffer, position in buffer, current value after 0)
--   to do the work here, so we strip all else
type CircularBufferState = (Int, Int, Int)


part2 :: Int -> Int
part2 inp = run2 inp 50000000


part1 :: Int -> Int
part1 inp = getAfter 2017 $ run inp 2017


run2 :: Int -> Int -> Int
run2 key cnt = go cnt 1 (1,0,0)
  where
    go 0 _ (_,_,c) = c
    go !n v !st    = let st' = step2 key st v in go (n-1) (v+1) st'


step2 :: Int -> CircularBufferState -> Int -> CircularBufferState
step2 key (!len, !pos, !cur) val =
      let insPos = (pos + key) `mod` len
          len'   = len + 1
          pos'   = insPos + 1
          cur'   = if insPos == 0 then val else cur
      in (len', pos', cur')


----------------------------------------------------------------------
-- part 1

data CircularBuffer =
  CB { bufferContent :: S.Seq Int
     , bufferIndex   :: Int
     , bufferLength  :: Int
     } deriving Show



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
