module Level18.Problem429
  ( problem
  ) where

import Data.IntMap (empty, fromList, unionWith)
import Data.Numbers.Primes (primeFactors, primes)

import Problem

-- http://mathworld.wolfram.com/UnitaryDivisorFunction.html
-- Tells us that for n its unitary divisors function is
-- (1+p1^(k.a1))(1+p2^(k.a2))...
-- where the ps are prime factors and the as are the prime factors' powers
-- For a factorial number n! its prime factors are gonna be less than n itself.
-- The problem remains getting the prime factors of a factorial. My first
-- approach of factoring all numbers from 1 to n was horribly slow. However
-- googling prime factorization of factorials leads us to Legendre's theorem
-- which states:
-- For any prime number p and any positive integer n, let Vp(n) be the exponent
-- of the largest power of p that divides n (that is, the p-adic valuation of
-- n). Then:
-- Vp(n!) = sum from i to infinity of floor (n/p^i).
-- And there we go, it's all done. The final algorithm runs in abouts 60
-- seconds. Again this may be worth revisiting to try to weed out some
-- optimization
problem :: Problem Integer
problem =
  Problem
    429
    "Sum of squares of unitary divisors"
    (fromIntegral $ sumUnitaryDivisorsSquared $ 10 ^ 8)

sumUnitaryDivisorsSquared :: Int -> Int
sumUnitaryDivisorsSquared n =
  foldl
    (\prev (p, exp) ->
       (prev * (1 + powmod p (exp * 2) (10 ^ 9 + 9))) `mod` (10 ^ 9 + 9))
    1 $
  primeFactorsOfFactorial n

powmod :: (Ord t, Integral p, Num t) => p -> t -> p -> p
powmod n exp m
  | exp < 1 = 1
  | otherwise = (n * powmod n (exp - 1) m) `mod` m

primeFactorsOfFactorial :: Integral a => a -> [(a, a)]
primeFactorsOfFactorial n =
  (\p -> (p, legendreFactorize p n)) <$> takeWhile (< n) primes

legendreFactorize :: Integral a => a -> a -> a
legendreFactorize d n = sum $ takeWhile (> 0) $ (n `div`) . (d ^) <$> [1 ..]
