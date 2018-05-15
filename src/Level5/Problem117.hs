module Level5.Problem117
  ( problem
  ) where

import Data.Array

import Problem

-- For problem 116 we used the binomianl coefficient to calculate how many
-- combinations are possiblem it is likely we can solve this problem using a
-- similar method but we can also do it using linear programming, so let's 
-- do that instead.
-- Given algorithm runs in O(n*m) time where n is the maximum number of units
-- and m is the number of tiles provided.
-- You gots to love you some dynamic programming.
problem :: Problem Integer
problem =
  Problem 117 "Red, green and blue tiles" (possibleCombinations 50 [1 .. 4])

possibleCombinations :: Integer -> [Integer] -> Integer
possibleCombinations maxUnits tiles = r ! maxUnits
  where
    r = listArray (0, maxUnits) (map f [0 .. maxUnits])
    f u = sum $ map (f' u) tiles
    f' u t
      | t == u = 1
      | t > u = 0
      | otherwise = r ! (u - t)
