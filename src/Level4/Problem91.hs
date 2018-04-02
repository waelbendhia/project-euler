module Level4.Problem91
  ( problem
  ) where

import Problem

problem :: Problem Integer
problem =
  Problem
  { ind = 91
  , name = "Right triangles with integer coordinates"
  , solution = toInteger $ length $ trianglesInBound (50 :: Integer)
  }

-- This brute force algorithm was only meant to check further solutions but it
-- turned out to be fast enough. I hope to revisit this problem at some point.
trianglesInBound :: Integral a => a -> [((a, a), (a, a))]
trianglesInBound b =
  filter (uncurry formsRightTriangle) $
  [(convert n1, convert n2) | n1 <- [1 .. hi], n2 <- [n1 + 1 .. hi]]
  where
    hi = (b + 1) ^ 2 - 1
    convert n = (div n (b + 1), rem n (b + 1))

formsRightTriangle :: (Num a, Eq a) => (a, a) -> (a, a) -> Bool
formsRightTriangle (x1, y1) (x2, y2) =
  x1 * x2 == -y1 * y2 ||
  x1 ^ 2 + y1 ^ 2 == x1 * x2 + y1 * y2 || x2 ^ 2 + y2 ^ 2 == x1 * x2 + y1 * y2
