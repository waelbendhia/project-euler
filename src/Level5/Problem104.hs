module Level5.Problem104
  ( problem
  ) where

import Data.Char
import Data.List
import Data.Maybe
import Problem

problem :: Problem Integer
problem =
  Problem
  { ind = 104
  , name = "Pandigital Fibonacci ends"
  , solution =
      fromMaybe 0 $
      findIndex problemCheck fibonacciSequence >>= Just . toInteger
  }

fibonacciSequence :: [Integer]
fibonacciSequence =
  0 : 1 : zipWith (+) fibonacciSequence (tail fibonacciSequence)

containsRepeatingDigits :: (Show a, Integral a) => a -> Bool
containsRepeatingDigits x =
  (length $ nub $ map digitToInt $ show x) /= (length $ show x)

isPandigital :: (Show a, Integral a) => a -> Bool
isPandigital n =
  (9 == (length $ show n)) &&
  (not $ containsRepeatingDigits n) && (not $ any (== '0') $show n)

problemCheck :: (Show a, Integral a) => a -> Bool
problemCheck x = isPandigital last9 && isPandigital first9
  where
    first9 = read $ take 9 $ show x
    last9 = x `rem` 10 ^ 9
