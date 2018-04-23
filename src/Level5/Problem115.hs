module Level5.Problem115
  ( problem
  ) where

import Data.Array

import Problem

-- dumbFunction m simply iterates recalculating fillCount m for each value
-- until a result exceeds 1000000. Runs pretty fast but the algorithm can be
-- made to run in (I think) linear time. I'm just not interested enough in this
-- problem to do it.
problem :: Problem Integer
problem =
  Problem
  { ind = 115
  , name = "Counting block combinations II"
  , solution = dumbFunction 50
  }

fillCount :: (Enum p, Num p, Num e, Ix p) => p -> p -> e
fillCount m n = r ! n
  where
    tiles = 1 : [m .. n]
    r = listArray (0, n) (map f [0 .. n])
    f u = sum $ map (f' u) tiles
    f' u t
      | t == u = 1
      | t /= 1 && t == u - 1 = 1
      | t > u = 0
      | t == 1 = r ! (u - t)
      | otherwise = r ! (u - t - 1)

dumbFunction :: (Enum a, Ix a, Num a) => a -> a
dumbFunction m = head $ dropWhile ((< 10 ^ 6) . fillCount m) [1 ..]
