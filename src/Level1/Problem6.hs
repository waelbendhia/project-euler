module Level1.Problem6
  ( problem
  ) where

import Problem

problem :: Problem Integer
problem =
  Problem {ind = 6, name = "Sum square difference", solution = solver 100}

solver :: Integer -> Integer
solver bound = abs ((sum $ map (^ 2) [1 .. bound]) - ((sum [1 .. bound]) ^ 2))
