module Level16.Problem387
  ( problem
  ) where

import Data.Char
import Data.Numbers.Primes (isPrime)

import Problem

-- Start by generating Right Truncatable Harshad Number of n-1 digits,
-- then filter the ones that are Strong Harshad Numbers, then append to all of
-- those odd numbers and keep the ones that are prime.
-- For this problem we're using trial division to test for primality as 
-- generating a look up table is not worth gain.
problem :: Problem Integer
problem =
  Problem 387 "Harshad Numbers" (sum $ strongRightTruncatableHarshadPrimes 14)

sumDigits :: Show a => a -> Integer
sumDigits = toInteger . sum . map digitToInt . show

isHarshad :: Integer -> Bool
isHarshad n = n < 10 || n `rem` sumDigits n == 0

rightTruncatableHarshads :: (Num t, Eq t) => t -> [Integer]
rightTruncatableHarshads 1 = [0 .. 9]
rightTruncatableHarshads digits = do
  c <- rightTruncatableHarshads (digits - 1)
  c * 10 : (filter isHarshad $ map (+ c * 10) [1 .. 9])

strongRightTruncatableHarshads :: (Eq t, Num t) => t -> [Integer]
strongRightTruncatableHarshads =
  filter (\c -> c /= 0 && isPrime (c `quot` sumDigits c)) .
  rightTruncatableHarshads

strongRightTruncatableHarshadPrimes :: (Eq t, Num t) => t -> [Integer]
strongRightTruncatableHarshadPrimes digits = do
  c <- strongRightTruncatableHarshads $ digits - 1
  filter isPrime $ map (+ c * 10) [1,3 .. 9]
