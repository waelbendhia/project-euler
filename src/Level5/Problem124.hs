module Level5.Problem124
  ( problem
  ) where

import Data.List
import Data.Numbers.Primes

import Problem

problem :: Problem Integer
problem =
  Problem
  { ind = 124
  , name = "Ordered radicals"
  , solution = orderByRad [1 .. 100000] !! 9999
  }

rad :: Integral a => a -> a
rad n = product $ nub $ primeFactors n

orderByRad :: [Integer] -> [Integer]
orderByRad =
  map fst . sortBy (\a b -> compare (snd a) (snd b)) . map (\n -> (n, rad n))
