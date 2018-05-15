module Level1.Problem21
  ( problem
  ) where

import Problem

problem :: Problem Integer
problem = Problem 21 "Amicable numbers" (sumAllAmicableNumbersUnder 10000)

sumAllAmicableNumbersUnder :: Integer -> Integer
sumAllAmicableNumbersUnder x = sum $ filter isAmicable [1 .. x]

sumDivisors :: Integer -> Integer
sumDivisors = (+ 1) . sum . divisors

divisors :: Integer -> [Integer]
divisors n = filter (/= n) $ filtered ++ map (quot n) filtered
  where
    filtered = filter ((==) 0 . rem n) [2 .. intSqrt n]
    intSqrt = truncate . sqrt . fromIntegral

isAmicable :: Integer -> Bool
isAmicable x = (sumDivisors $ sumDivisors x) == x && sumDivisors x /= x
