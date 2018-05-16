module Level4.Problem85
  ( problem
  ) where

import Data.List

import Problem

problem :: Problem Integer
problem = Problem 85 "Counting rectangles" (findClosest 2000000)

findClosest :: Integral b => b -> b
findClosest cRect =
  fst $
  minimumBy ((. snd) . compare . snd) $
  map (\(w, h) -> (w * h, abs $cRect - count w h)) rects
  where
    b = bound cRect
    rects = concatMap (\w -> map ((,) w) [w + 1 .. b]) [1 .. b]

bound :: Integral a => a -> a
bound cRect = (2 *) $ head $ dropWhile (\d -> count d d < cRect) [1 ..]

count :: Integral a => a -> a -> a
count w h = sumTo w * sumTo h

sumTo :: Integral a => a -> a
sumTo n = (n ^ 2 + n) `div` 2
