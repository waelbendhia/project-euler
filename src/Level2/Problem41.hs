module Level2.Problem41
  ( problem
  ) where

import Data.List
import Data.Maybe
import Data.Numbers.Primes

import Problem

problem :: Problem Integer
problem = Problem 41 "Pandigital prime" biggestPandigitalPrime

biggestPandigitalPrime :: Integer
biggestPandigitalPrime = helper 9
  where
    helper n
      | n < 1 = 0
      | otherwise =
        fromMaybe (helper $ n - 1) $ find isPrime $ generateNPandigital n

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
           map (\y -> x * (10 ^ numDigits y) + y) $
           genPandigital $ filter (/= x) l)
        l
