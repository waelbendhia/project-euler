module Level21.Problem516
  (
  ) where

import Data.List
import Math.NumberTheory.Primes.Testing (isPrime)

import Problem

-- A little analysis, let's call Euler's totient function T. For a prime power
-- we have:
-- T(p^k) = (p^(k-1)).(p-1)
-- If two numbers are relative m and n are coprime then we have:
-- T(m.n) = T(m).T(n)
-- So for a Hamming number its totient is:
-- T(2^a.3^b.5^c) = 2^(a-1).(2 - 1).3^(a-1).(3 - 1).5^(a-1).(5 - 1)
-- T(2^a.3^b.5^c) = 2^(a-1).3^(a-1).2.5^(a-1).2^2
-- T(2^a.3^b.5^c) = 2^(a+2).3^(a-1).5^(a-1)
-- Also a Hamming number!
-- However this is not enough we're still missing a bunch of numbers.
-- Taking that set of Hamming producing primes, if we multiply them together
-- we get another Hamming totient number.
-- We're still missing some Hamming totient numbers. So if we multiply all our
-- number by all our Hamming numbers we should generate a larger set of Hamming
-- totient numbers.
-- Since trial division is kinda slow I decided to use a non-deterministic
-- primality test.
problem :: Problem Integer
problem = Problem 516 "5-smooth totients" (0)

generateHammingNumbersUnder b = do
  two <- takeWhile (< b) $ (2 ^) <$> [0 ..]
  three <- takeWhile (< b) $((two *) . (3 ^)) <$> [0 ..]
  takeWhile (< b) $((three *) . (5 ^)) <$> [0 ..]

hammingTotientPrimesUnder :: Integer -> [Integer]
hammingTotientPrimesUnder b = filter isPrime $ (+ 1) <$> hammings
  where
    hammings = generateHammingNumbersUnder b

hammingTotientNumbersUnder b = do
  n <- hammings
  filter (< b) $ (* n) <$> prods
  where
    hammings = generateHammingNumbersUnder b
    htPrimes = filter (\x -> isPrime x && x > 5) $ (+ 1) <$> hammings
    prods = allProducts htPrimes b

allProducts l b = helper l
  where
    helper [] = []
    helper (x:xs) = x : helper (xs ++ filter (< b) ((* x) <$> xs))
