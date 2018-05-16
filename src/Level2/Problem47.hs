module Level2.Problem47
  ( problem
  ) where

import Data.Numbers.Primes

import Problem

problem :: Problem Integer
problem =
  Problem 47 "Distinct primes factors" (head $ nConsecutiveWithNPrimeFactors 4)

nConsecutiveWithNPrimeFactors :: Int -> [Integer]
nConsecutiveWithNPrimeFactors n = helper [1 ..]
  where
    helper l@(x:_)
      | numDistincPrimeFactors x == n =
        if length nPrimed == n
          then nPrimed
          else helper rest
      | otherwise = helper $ dropWhile ((/= n) . numDistincPrimeFactors) l
      where
        (nPrimed, rest) = span ((== n) . numDistincPrimeFactors) l

numDistincPrimeFactors :: Integer -> Int
numDistincPrimeFactors = length . remConsDup . primeFactors

remConsDup :: Eq a => [a] -> [a]
remConsDup =
  foldl
    (\p c ->
       if not (null p) && head p == c
         then p
         else c : p)
    []
