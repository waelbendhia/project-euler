module Level2.Problem44
  ( problem
  ) where

import Problem

problem :: Problem Integer
problem =
  Problem
  { ind = 44
  , name = "Pentagon numbers"
  , solution = subT $ head $ allSuperPentagonal
  }

allSuperPentagonal :: [(Integer, Integer)]
allSuperPentagonal =
  filter ((/= Nothing) . pentagonalTerm . subT) $
  filter ((/= Nothing) . pentagonalTerm . addT) $
  concatMap
    (\a -> map ((,) (pentagonalN a)) (map pentagonalN [1 .. a - 1]))
    [1 ..]

subT :: Num a => (a, a) -> a
subT (a, b) = abs (a - b)

addT :: Num a => (a, a) -> a
addT (a, b) = a + b

intSqrt :: Integral a => a -> Maybe a
intSqrt n =
  if n < 0
    then Nothing
    else if root ^ 2 == n
           then Just root
           else Nothing
  where
    root = truncate $ sqrt $ fromIntegral n

pentagonalTerm :: Integral a => a -> Maybe a
pentagonalTerm p =
  discriminant >>= \d ->
    Just
      (if 1 + d > 0
         then 1 + d
         else 1 - d) >>= \x ->
      if rem x 6 == 0
        then Just (div x 6)
        else Nothing
  where
    discriminant = intSqrt (1 + 24 * p)

pentagonalN :: Integral a => a -> a
pentagonalN n = div (3 * n ^ 2 - n) 2
