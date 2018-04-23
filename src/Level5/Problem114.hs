module Level5.Problem114
  ( problem
  ) where

import Data.Array

import Problem

-- Same as 117 except for some extra checks
problem :: Problem Integer
problem =
  Problem
  { ind = 114
  , name = "Counting block combinations I"
  , solution = possibleCombinations 50
  }

possibleCombinations maxUnits = r ! maxUnits
  where
    tiles = 1 : [3 .. maxUnits]
    r = listArray (0, maxUnits) (map f [0 .. maxUnits])
    f u = sum $ map (f' u) tiles
    f' u t
      | t == u = 1
      | t /= 1 && t == u - 1 = 1
      | t > u = 0
      | t == 1 = r ! (u - t)
      | otherwise = r ! (u - t - 1)
