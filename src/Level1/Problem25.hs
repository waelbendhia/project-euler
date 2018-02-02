module Level1.Problem25
  ( problem
  ) where

import Problem

problem :: Problem Integer
problem =
  Problem
  { ind = 25
  , name = "1000-digit Fibonacci number"
  , solution = toInteger $ firstFibOfLength 1000
  }

firstFibOfLength :: Int -> Int
firstFibOfLength n =
  length $ takeWhile ((>) n . length . show) fibonacciSequence

fibonacciSequence :: [Integer]
fibonacciSequence =
  0 : 1 : zipWith (+) fibonacciSequence (tail fibonacciSequence)
