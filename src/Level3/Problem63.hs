module Level3.Problem63
  ( problem
  ) where

import Problem

problem :: Problem Integer
problem =
  Problem 63 "Powerful digit counts" (toInteger $ length $ allPowerfulDigits)

allPowerfulDigits :: [Integer]
allPowerfulDigits = concatMap f [1 .. 9]
  where
    f x =
      map (x ^) $
      filter (\n -> numDigits (x ^ n) == n) [1 .. highestPossiblePower]

-- For any number above 9 x x^n will have more than n digits
highestPossiblePower :: Int
highestPossiblePower = helper 1
  where
    helper n =
      if numDigits (9 ^ n) == n
        then helper (n + 1)
        else n - 1

numDigits :: (Show a, Integral a) => a -> Int
numDigits = length . show
