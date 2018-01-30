module Problem6
  ( problem6
  ) where

import Problem

problem6 :: Problem Integer
problem6 = Problem {ind = 6, name = "Sum square difference", solution = sol}

sol :: Integer
sol = abs ((sum $ map (^ 2) [1 .. 100]) - ((sum [1 .. 100]) ^ 2))
