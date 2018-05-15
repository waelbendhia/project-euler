module Level2.Problem30
  ( problem
  ) where

import Data.Char

import Problem

problem :: Problem Integer
problem =
  Problem
    30
    "Digit fifth powers"
    (fromIntegral $sum $ findMatchingSumOfDigitPowers 5)

findMatchingSumOfDigitPowers :: Integral b => b -> [Int]
findMatchingSumOfDigitPowers p =
  filter (isSumOfDigitPowers p) [2 .. upperBound p]

isSumOfDigitPowers :: Integral b => b -> Int -> Bool
isSumOfDigitPowers p n = n == (sum $ digitPowers p n)

digitPowers :: (Show a, Integral b) => b -> a -> [Int]
digitPowers n = map ((^ n) . digitToInt) . show

numDigits :: Int -> Int
numDigits = length . show

-- We need to find n so as numer of digits in n*9^p == n
-- We know that beyond this value all sums of powers of p will not be equals to x
lenThatProducesSameLen :: Integral t => t -> Int
lenThatProducesSameLen p = loop minLen
  where
    minLen = numDigits (9 ^ p)
    loop x
      | numDigits (x * (9 ^ p)) == x = x
      | otherwise = numDigits (x * (9 ^ p))

upperBound :: Integral b => b -> Int
upperBound p = maxLen * (9 ^ p)
  where
    maxLen = lenThatProducesSameLen p
