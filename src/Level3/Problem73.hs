module Level3.Problem73
  ( problem
  ) where

import Problem

problem :: Problem Integer
problem =
  Problem
    73
    "Counting fractions in a range"
    (toInteger $ uniqueFractionsInRange (1, 3) (1, 2) 12000)

multiply :: Integral t => (t, t) -> t -> (t, t)
multiply (n, d) x = ((x * n) `div` d, (x * n) `rem` d)

uniqueFractionsInRange :: Integral a => (a, a) -> (a, a) -> a -> Int
uniqueFractionsInRange minVal maxVal n =
  sum $
  map
    (\x ->
       let floorVal = (+ 1) $ fst $ multiply minVal x
           ceilingVal =
             case multiply maxVal x of
               (d, 0) -> d - 1
               (d, _) -> d
        in phiRange floorVal ceilingVal x)
    [1 .. n]

phiRange :: Integral a => a -> a -> a -> Int
phiRange minVal maxVal n = length $ filter (coprime n) [minVal .. maxVal]

coprime :: Integral a => a -> a -> Bool
coprime = ((== 1) .) . gcd
