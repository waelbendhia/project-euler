module Level3.Problem70
  ( problem
  ) where

import Data.List
import Data.Numbers.Primes

import Problem

-- When compiled this yields a result in about a minute, there should be
-- more optimizations possible, but hey it works.
problem :: Problem Integer
problem =
  Problem
    70
    "Totient permutation"
    (minimumBy compareByInvTotient $ filter isPhiPermutation [2 .. 10000000])

compareByInvTotient :: (Integral a, Integral a1) => a1 -> a -> Ordering
compareByInvTotient a b = compare (invTotient a) (invTotient b)

invTotient :: (Integral a1, Fractional a) => a1 -> a
invTotient n = fromIntegral n / fromIntegral (phi n)

phi :: Integral b => b -> b
phi = product . map f . group . primeFactors
  where
    f l@(p:_) = p ^ length l - p ^ (length l - 1)

isPhiPermutation :: (Integral b, Show b) => b -> Bool
isPhiPermutation a = shareDigits a (phi a)

shareDigits :: Show a => a -> a -> Bool
shareDigits a b = f a == f b
  where
    f = sort . show
