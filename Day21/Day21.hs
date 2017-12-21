{-# LANGUAGE TupleSections, BangPatterns #-}
-- Solution to Day 21 of the Advent Of Code 2017

module Main where

import qualified Data.Set as S
import Data.List (find)


main :: IO ()
main = do
  inp <- readInput
  putStrLn $ "solve me"


type Input = ()

data Image =
  Image
  { dim  :: Int
  , pixels :: S.Set Coord
  } deriving Show

type Coord = (Int,Int)

type Pattern = (Int, [String])

type Rule = (Pattern, Pattern)

type Transform = (Square, Square)

data Square =
  Square
  { squareDim :: Int
  , squarePixels :: S.Set Coord
  } deriving Show


instance Eq Square where
  (Square da pa) == (Square db pb) = da == db && pa == pb


startImage :: Image
startImage = Image 3 (S.fromList pxls)
  where pxls = [ (1,0), (2,1), (0,2), (1,2), (2,2) ]


transformImage :: [Transform] -> Image -> Image
transformImage ts img =
  let offSqs = squares img
      trsSqs = map (fmap (transformSquare ts)) offSqs
      newCoords = concatMap (uncurry squareToImgCoords) trsSqs
      newDim = if even (dim img) then (dim img `div` 2) * 3 else (dim img `div` 3) * 4
  in Image newDim (S.fromList newCoords)


squares :: Image -> [(Coord, Square)]
squares img
  | even (dim img) = squares2 img
  | otherwise      = squares3 img


transformSquare :: [Transform] -> Square -> Square
transformSquare ts sq = snd $ head $ filter ((== sq) . fst) ts


matchingSquares :: Square -> [Square]
matchingSquares sq = map (flip mapSquare sq) defaultTransforms


mapSquare :: (Coord -> Coord) -> Square -> Square
mapSquare t (Square d coords) = Square d (S.map t coords)


squares2 :: Image -> [(Coord, Square)]
squares2 img = map (getSquare2 img) [ (x,y) | y <- [0,2..dim img - 1], x <- [0,2..dim img -1] ]


squares3 :: Image -> [(Coord, Square)]
squares3 img = map (getSquare3 img) [ (x,y) | y <- [0,3..dim img - 1], x <- [0,3..dim img -1] ]


getSquare2 :: Image -> Coord -> (Coord, Square)
getSquare2 img off@(offX,offY) = (newOff,) $ Square 2 $ S.fromList $ concat
  [ [(x*2-1, y*2-1) | x <- [0,1], getPixel img (offX+x, offY+y) ] | y <- [0,1] ]
  where newOff = ((offX `div` 2) * 3, (offY `div` 2) * 3)


getSquare3 :: Image -> Coord -> (Coord, Square)
getSquare3 img off@(offX,offY) = (newOff,) $ Square 3 $ S.fromList $ concat
  [ [(x-1, y-1) | x <- [0..2], getPixel img (offX+x, offY+y) ] | y <- [0..2] ]
  where newOff = ((offX `div` 3) * 4, (offY `div` 3) * 4)


squareToImgCoords :: Coord -> Square -> [Coord]
squareToImgCoords off (Square 2 cs) = square2toImgCoords off (S.toList cs)
squareToImgCoords off (Square 3 cs) = square3toImgCoords off (S.toList cs)
squareToImgCoords off (Square 4 cs) = square4toImgCoords off (S.toList cs)


square2toImgCoords :: Coord -> [Coord] -> [Coord]
square2toImgCoords (offX,offY) cs = [ (offX + ((x+1) `div` 2), offY + ((y+1) `div` 2)) | (x,y) <- cs ]


square3toImgCoords :: Coord -> [Coord] -> [Coord]
square3toImgCoords (offX,offY) cs = [ (offX+x+1, offY+y+1) | (x,y) <- cs ]


square4toImgCoords :: Coord -> [Coord] -> [Coord]
square4toImgCoords (offX,offY) cs = [ (offX + m x, offY + m y) | (x,y) <- cs ]
  where m (-2) = 0
        m (-1) = 1
        m 1    = 2
        m 2    = 3



ruleToTransforms :: Rule -> [Transform]
ruleToTransforms (match, new) =
  let matchSq = patternToSquare match
      newSq = patternToSquare new
  in (,newSq) <$> matchingSquares matchSq


patternToSquare :: Pattern -> Square
patternToSquare (2, pxs) =
  let withCoords = map (\ (y,ps) -> map (\ (x,c) -> ((x,y), c)) $ zip [-1,1] ps ) $ zip [-1,1] pxs
  in Square 2 $ S.fromList $ concatMap (map fst . filter ((== '#') . snd)) withCoords
patternToSquare (3, pxs) =
  let withCoords = map (\ (y,ps) -> map (\ (x,c) -> ((x,y), c)) $ zip [-1,0,1] ps ) $ zip [-1,0,1] pxs
  in Square 3 $ S.fromList $ concatMap (map fst . filter ((== '#') . snd)) withCoords
patternToSquare (4, pxs) =
  let withCoords = map (\ (y,ps) -> map (\ (x,c) -> ((x,y), c)) $ zip [-2,-1,1,2] ps ) $ zip [-2,-1,1,2] pxs
  in Square 4 $ S.fromList $ concatMap (map fst . filter ((== '#') . snd)) withCoords



defaultTransforms :: [Coord -> Coord]
defaultTransforms = [ id, rot90, rot180, rot270, flipX, flipY ]
  where rot90 = rotate
        rot180 = rot90 . rot90
        rot270 = rot90 . rot180


rotate :: Coord -> Coord
rotate (x,y) = (negate y, x)


flipX :: Coord -> Coord
flipX (x,y) = (negate x, y)


flipY :: Coord -> Coord
flipY (x,y) = (x, negate y)


translate :: Coord -> Coord -> Coord
translate (dx,dy) (x,y) = (x+dx,y+dy)


getPixel :: Image -> Coord -> Bool
getPixel img = (`S.member` pixels img)


imageLines :: Image -> [[Bool]]
imageLines img = [ [ getPixel img (x,y) | x <- [0..dim img - 1] ] | y <- [0..dim img - 1] ]


printImage :: Image -> IO ()
printImage img = mapM_ printLine (imageLines img)
  where printLine xs = putStrLn $ (map toChar xs)
        toChar False = '.'
        toChar True  = '#'


nTimes :: Int -> (a -> a) -> a -> a
nTimes 0 _ !x = x
nTimes n f !x = nTimes (n-1) f (f x)


exampleRules :: [Rule]
exampleRules =
  [ ((2, ["..",".#"]), (3,["##.","#..","..."]))
  , ((3, [".#.","..#","###"]), (4,["#..#","....","....","#..#"]))
  ]


exampleTransforms :: [Transform]
exampleTransforms = concatMap ruleToTransforms exampleRules


readInput :: IO Input
readInput = const () <$> readFile "input.txt"
