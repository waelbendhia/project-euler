module Level3.Problem69
  ( problem
  ) where

import Data.List
import Data.Numbers.Primes

import Problem

problem :: Problem Integer
problem =
  Problem 69 "Totient maximum" (maximumBy compareByInvTotient [2 .. 1000000])

compareByInvTotient :: (Integral a, Integral a1) => a1 -> a -> Ordering
compareByInvTotient a b = compare (invTotient a) (invTotient b)

invTotient :: (Integral a1, Fractional a) => a1 -> a
invTotient n = fromIntegral n / fromIntegral (phi n)

-- According to wikipedia if m and n are coprime then phi(m*n) = phi(m)*phi(n)
-- and if if n is a power of a prime p then phi(p^k) = p^k - p^(k-1) hence this
-- function
phi :: Integral b => b -> b
phi = product . map f . group . primeFactors
  where
    f l@(p:_) = p ^ (length l) - p ^ (length l - 1)
