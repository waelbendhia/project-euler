module Level1.Problem2
  ( problem
  ) where

import Problem

problem :: Problem Integer
problem =
  Problem
    2
    "Even Fibonacci numbers"
    (sum $ filter even $ takeWhile (<= 4000000) fibonacciSequence)

fibonacciSequence :: [Integer]
fibonacciSequence =
  0 : 1 : zipWith (+) fibonacciSequence (tail fibonacciSequence)
