module Level1.Problem5
  ( problem
  ) where

import Problem

problem :: Problem Integer
problem =
  Problem {ind = 5, name = "Smallest multiple", solution = smallestMultiple 20}

smallestMultiple :: Integral p => p -> p
smallestMultiple 0 = 1
smallestMultiple n =
  head $
  filter ((== 0) . (`rem` n)) $ map (* smallestMultiple (n - 1)) $ divisors n
  where
    divisors m =
      let divs x
            | x * x > m = []
            | m `rem` x == 0 = x : divs (x + 1)
            | otherwise = divs (x + 1)
      in divs 1 ++ [m]
