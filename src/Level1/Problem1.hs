module Level1.Problem1
  ( problem
  ) where

import Problem

problem :: Problem Integer
problem = Problem 1 "Multiples of 3 and 5" (multiplesOf [3, 5] 1000)

multiplesOf :: (Integral a, Foldable t) => t a -> a -> a
multiplesOf l under =
  sum $ filter (\y -> any (\x -> rem y x == 0) l) [1 .. under - 1]
