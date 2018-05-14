module Level16.Problem381
  ( problem
  ) where

import Data.Maybe
import Data.Numbers.Primes

import Problem

problem :: Problem Integer
problem =
  Problem
    {ind = 381, name = "(prime-k) factorial", solution = sumCapitalS (10 ^ 8)}

-- S(p) = (p-5)! + (p-4)! + (p-3)! + (p-2)! + (p-1)!
-- Since we're in modulo p we can simplify to
-- S(p) = (p-5)! * ( 1  - 4 + 12 + -24 + 24)
-- S(p) = 9*(p-5)!
-- This gets us pretty close but still computing (10^8 - 5)! is not optimal.
-- After a lot of frustration I decided to check if there was a common pattern
-- for any of the factorials and I noticed that for all p prime:
-- (p-1)! % p = p-1
-- which googling it turns up Wilson's theorem which states:
-- (p-1)! (mod p) = -1 (mod p)
-- With this further simplification is possible:
-- S(p) = 9 * (p-1)!/((p-4)*(p-3)*(p-2)*(p-1))
-- S(p) = 9 * (p-1)!/((-4)*(-3)*(-2)*(-1))
-- S(p) = 9 * (p-1)!/24
-- S(p) = 3 * (p-1)!/8
-- S(p) = -3/8 (mod p)
-- After copying the modulo inverse function from here:
-- https://rosettacode.org/wiki/Modular_inverse#Haskell
-- We get the following algorithm:
sumCapitalS :: Integral a => a -> a
sumCapitalS n = sum $ map capitalS $ takeWhile (< n) $ drop 2 primes

capitalS :: Integral a => a -> a
capitalS p = (-3 * (fromMaybe 0 $ modInv 8 p)) `mod` p

-- This is the first algorithm I implemented which works okay enough up to 10^5
-- sumCapitalS :: Integral p => p -> p
-- sumCapitalS n = helper 5 1 (drop 2 primes)
--   where
--     helper last fact (p:ps)
--       | p >= n = 0
--       | otherwise = (9 * newFact `mod` p) + helper p newFact ps
--       where
--         newFact = product [last - 4 .. p - 5] * fact
-- From: https://rosettacode.org/wiki/Modular_inverse#Haskell
-- Extended Euclidean algorithm.  Given non-negative a and b, return x, y and g
-- such that ax + by = g, where g = gcd(a,b).  Note that x or y may be negative.
gcdExt a 0 = (1, 0, a)
gcdExt a b =
  let (q, r) = a `quotRem` b
      (s, t, g) = gcdExt b r
   in (t, s - q * t, g)

-- Given a and m, return Just x such that ax = 1 mod m.  If there is no such x
-- return Nothing.
modInv a m =
  let (i, _, g) = gcdExt a m
   in if g == 1
        then Just (mkPos i)
        else Nothing
  where
    mkPos x =
      if x < 0
        then x + m
        else x
