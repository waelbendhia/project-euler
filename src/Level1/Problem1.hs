module Level1.Problem1
  ( problem
  ) where

import Problem

problem :: Problem Integer
problem = Problem 1 "Multiples of 3 and 5" (sumMultiples2 3 5 100)

sumMultiples2 :: Integral p => p -> p -> p -> p
sumMultiples2 x y b =
  sumMultiples x b + sumMultiples y b - sumMultiples (x * y) b

sumMultiples :: Integral p => p -> p -> p
sumMultiples x b = (nTerms * (x + x * nTerms)) `div` 2
  where
    nTerms = (b - 1) `div` x
