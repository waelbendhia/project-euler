module Level4.Problem86
  ( problem
  ) where

import Problem

problem :: Problem Integer
problem =
  Problem {ind = 86, name = "Cuboid Route", solution = firstAbove 1000000}

-- The basic idea is to fix a the width of the cuboid and then check all
-- values 2 <= v <= w for which sqrt(w^2+v^2) is an integer.
-- And for each v we find we know that there are v / 2 pairs of (a,b) for which
-- a+b == v and a > 0 and b > 0. Except for when v > hl then we only consider pairs
-- for which a =< w and a =< w which after some pen and paper testing and messing
-- around turns out to be 1 + w - (v+q)/2 pairs.
allLengths :: [Integer]
allLengths = scanl (\p c -> p + numberOfIntCuboidsProduced c) 0 [1 ..]

firstAbove :: Num t => Integer -> t
firstAbove n = helper' 0 allLengths
  where
    helper' x nums =
      if head nums > n
        then x
        else helper' (x + 1) (tail nums)

numberOfIntCuboidsProduced :: Integral t => t -> t
numberOfIntCuboidsProduced w = helper 2
  where
    helper hl =
      (+)
        (if intSqrt (hl ^ 2 + w ^ 2)
           then if hl > w
                  then 1 + w - ((hl + 1) `div` 2)
                  else hl `div` 2
           else 0)
        (if w * 2 == hl
           then 0
           else helper (hl + 1))
      where


intSqrt :: Integral a => a -> Bool
intSqrt r = (truncate $ sqrt $ fromIntegral r) ^ 2 == r
