module Level5.Problem108
  ( problem
  ) where

import Math.NumberTheory.Primes.Factorisation

import Problem

-- Let's analyze
-- 1/x + 1/y = 1/n
-- The problem explanation says distinct solutions so we can set x <= y.
-- Second x, y and n are all positive integers so 1/x < 1/n and 1/y < 1/n
-- so y >= x > n
-- n(x + y) = xy
-- If we substitute x with n+a and y with n+b we get:
-- 2n^2 + na + nb = n^2 + nb + na + ab
-- n^2 = ab
-- So the number of distinct results is the same as the number of divisor pairs
-- of n^2 or half the number of divisors of n^2 + 1
problem :: Problem Integer
problem =
  Problem
    108
    "Diophantine reciprocals I"
    (head $ dropWhile (\n -> (numFactors (n ^ 2) + 1) `div` 2 < 1000) [1 ..])

numFactors :: Integer -> Int
numFactors x = foldl (\p (_, exp) -> (exp + 1) * p) 1 $ factorise x
