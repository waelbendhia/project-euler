module Problem1
  ( problem1
  ) where

import Problem

problem1 :: Problem Integer
problem1 =
  Problem {ind = 1, name = "Multiples of 3 and 5", solution = solver 1000}

solver :: Integer -> Integer
solver x = sum $ filter (\y -> rem y 3 == 0 || rem y 5 == 0) [1 .. (x - 1)]
