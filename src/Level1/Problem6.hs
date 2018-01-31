module Problem6
  ( problem6
  ) where

import Problem

problem6 :: Problem Integer
problem6 =
  Problem {ind = 6, name = "Sum square difference", solution = solver 100}

solver :: Integer -> Integer
solver bound = abs ((sum $ map (^ 2) [1 .. bound]) - ((sum [1 .. bound]) ^ 2))
