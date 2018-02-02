module Level1.Problem1
  ( problem
  ) where

import Problem

problem :: Problem Integer
problem =
  Problem {ind = 1, name = "Multiples of 3 and 5", solution = solver 1000}

solver :: Integer -> Integer
solver x = sum $ filter (\y -> rem y 3 == 0 || rem y 5 == 0) [1 .. (x - 1)]
