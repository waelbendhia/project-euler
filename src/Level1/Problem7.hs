module Level1.Problem7
  ( problem
  ) where

import Data.Array

import Problem

problem :: Problem Integer
problem = Problem 7 "10001st prime" (nthPrime 10001)

nthPrime :: (Integral e, Num a, Ix a, Enum a) => a -> e
nthPrime n = arr ! (n - 1)
  where
    arr = listArray (0, n - 1) (map f [0 .. n - 1])
    f x
      | x == 0 = 2
      | otherwise =
        head $
        filter
          (\c -> all (\p -> c `rem` p /= 0) lowerPrimes)
          [nextOdd,nextOdd + 2 ..]
      where
        prev = arr ! (x - 1)
        nextOdd =
          (prev +) $
          if even prev
            then 1
            else 2
        lowerPrimes = map (arr !) [0 .. x - 1]
