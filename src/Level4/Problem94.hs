module Level4.Problem94
  ( problem
  ) where

import Problem

problem :: Problem Integer
problem =
  Problem
  { ind = 94
  , name = "Almost equilateral triangle"
  , solution =
      sum $
      takeWhile (< 10 ^ 9) $ map almostEquiPermiter filteredParentChildTriplets
  }

filteredParentChildTriplets :: [(Integer, Integer, Integer)]
filteredParentChildTriplets =
  (3, 4, 5) : (filteredParentChildTriplets >>= genNext)
  where
    genNext (a, b, c) =
      filter
        isAlmostEquilateral
        [ (a + 2 * (c - b), 2 * (a + c) - b, 2 * (a - b) + 3 * c)
        , (a + 2 * (b + c), 2 * (a + c) + b, 2 * (a + b) + 3 * c)
        , (-a + 2 * (b + c), 2 * (c - a) + b, 2 * (b - a) + 3 * c)
        ]

almostEquiPermiter :: (Num a, Eq a) => (a, a, a) -> a
almostEquiPermiter (a, b, c) =
  if abs (2 * a - c) == 1
    then 4 * a + c
    else 4 * b + c

isAlmostEquilateral :: (Num a, Eq a) => (a, a, a) -> Bool
isAlmostEquilateral (a, b, c) = abs (2 * a - c) == 1 || abs (2 * b - c) == 1
