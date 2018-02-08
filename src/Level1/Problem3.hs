module Level1.Problem3
  ( problem
  ) where

import Problem

problem :: Problem Integer
problem =
  Problem
  { ind = 3
  , name = "Largest prime factor"
  , solution = firstPrime $ factors 600851475143
  }

factors :: Integer -> [Integer]
factors n = map (quot n) filtered ++ reverse filtered
  where
    filtered = filter ((==) 0 . rem n) [2 .. intSqrt n]

intSqrt :: Integer -> Integer
intSqrt x = truncate $ sqrt $ (fromIntegral x :: Double)

isPrime :: Integer -> Bool
isPrime a = isPrime' 2
  where
    isPrime' x
      | x > intSqrt a = True
      | rem a x == 0 = False
      | otherwise = isPrime' (x + 1)

firstPrime :: [Integer] -> Integer
firstPrime (x:xs) =
  if isPrime x
    then x
    else firstPrime xs
firstPrime [] = 0
