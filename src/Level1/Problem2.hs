module Level1.Problem2
  ( problem
  ) where

import Problem

problem :: Problem Integer
problem =
  Problem
  { ind = 2
  , name = "Even Fibonacci numbers"
  , solution = sum $ filter even $ takeWhile (<= 4000000) fibonacciSequence
  }

fibonacciSequence :: [Integer]
fibonacciSequence =
  0 : 1 : zipWith (+) fibonacciSequence (tail fibonacciSequence)
