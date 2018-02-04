module Level2.Problem37
  ( problem
  ) where

import Data.Numbers.Primes
import qualified Data.Set as S
import Problem

problem :: Problem Integer
problem =
  Problem
  {ind = 37, name = "Truncatable primes", solution = sum $ truncatablePrimes}

truncatablePrimes :: [Integer]
truncatablePrimes =
  take 11 $ filter isTruncatablePrime $ dropWhile (< 20) primes

isTruncatablePrime :: Integer -> Bool
isTruncatablePrime = all isPrime . allTruncations

allTruncations :: Integer -> [Integer]
allTruncations n = S.toList $ S.fromList $ truncsLeft n ++ truncsRight n

truncsLeft :: Integer -> [Integer]
truncsLeft n
  | n == 0 = []
  | n < 10 = [n]
  | otherwise = n : truncsLeft (n `rem` d)
  where
    d = 10 ^ (numDigits n - 1)

truncsRight :: Integer -> [Integer]
truncsRight n
  | n == 0 = []
  | n < 10 = [n]
  | otherwise = n : truncsRight (n `div` 10)

numDigits :: Integer -> Integer
numDigits = (+ 1) . truncate . logBase 10 . fromIntegral

intSqrt :: (Integral a, Integral b) => a -> b
intSqrt x = truncate $ sqrt $ (fromIntegral x :: Double)
