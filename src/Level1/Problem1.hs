module Problem1
  ( problem1
  ) where

import Problem

problem1 :: Problem Integer
problem1 = Problem {ind = 1, name = "Multiples of 3 and 5", solution = sol}

sol :: Integer
sol = sum $ filter (\x -> rem x 3 == 0 || rem x 5 == 0) [1 .. 999]
