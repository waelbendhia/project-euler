module Problem2
  ( problem2
  ) where

import Problem

problem2 :: Problem Integer
problem2 =
  Problem {ind = 2, name = "Even Fibonacci numbers", solution = solver 4000000}

solver :: Integer -> Integer
solver x = sum $ filter even $ takeWhile (<= x) fibonacciSequence
  where
    fibonacciSequence =
      0 : 1 : zipWith (+) fibonacciSequence (tail fibonacciSequence)
