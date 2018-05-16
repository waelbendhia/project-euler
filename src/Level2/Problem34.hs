module Level2.Problem34
  ( problem
  ) where

import Data.Char
import Problem

problem :: Problem Integer
problem = Problem 34 "Digit factorials" (toInteger $ sum correctNumbers)

correctNumbers :: [Int]
correctNumbers = filter check [3 .. upperBound]

upperBound :: Int
upperBound = loop 1 * factorial 9
  where
    loop n
      | length (show $ n * factorial 9) == n = n
      | otherwise = loop (n + 1)

check :: Int -> Bool
check n = n == digitSumFactorial n

digitSumFactorial :: Show a => a -> Int
digitSumFactorial = sum . map (factorial . digitToInt) . show

factorial :: Int -> Int
factorial = product . enumFromTo 1
