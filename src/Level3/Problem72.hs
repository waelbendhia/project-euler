module Level3.Problem72
  ( problem
  ) where

import Data.List
import Data.Numbers.Primes

import Problem

problem :: Problem Integer
problem = Problem 72 "Counting fractions" (uniqueFractionsUnder 1000000)

-- For each n the number of unique fractions m/n where m < n is the number of
-- coprimes m to n  plus the number of unique fractions under (n-1). Now if
-- only we had a function to calculate the number of coprimes to n less than n.
uniqueFractionsUnder :: Integral t => t -> t
uniqueFractionsUnder 1 = 0
uniqueFractionsUnder n = phi n + uniqueFractionsUnder (n - 1)

phi :: Integral b => b -> b
phi = product . map f . group . primeFactors
  where
    f l@(p:_) = p ^ length l - p ^ (length l - 1)
