module Level2.Problem41
  ( problem
  ) where

import Data.List
import Data.Numbers.Primes

import Problem

problem :: Problem Integer
problem =
  Problem
  {ind = 41, name = "Pandigital prime", solution = biggestPandigitalPrime}

biggestPandigitalPrime :: Integer
biggestPandigitalPrime = helper 9
  where
    helper n
      | n < 1 = 0
      | otherwise =
        case find isPrime $ generateNPandigital n of
          Nothing -> helper (n - 1)
          Just x -> x

numDigits :: Show a => a -> Int
numDigits = length . show

generateNPandigital :: (Enum t, Show t, Num t, Ord t) => t -> [t]
generateNPandigital n
  | n > 9 = generateNPandigital 9
  | n < 0 = generateNPandigital 1
  | otherwise = genPandigital [n,n - 1 .. 1]
  where
    genPandigital [m] = [m]
    genPandigital l =
      concatMap
        (\x ->
           map (\y -> x * (10 ^ (numDigits y)) + y) $
           genPandigital $ filter (/= x) l)
        l
