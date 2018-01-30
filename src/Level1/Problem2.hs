module Problem2
  ( problem2
  ) where

import Problem

problem2 :: Problem Integer
problem2 = Problem {ind = 2, name = "Even Fibonacci numbers", solution = sol}

sol :: Integer
sol = sum $ filter even $ takeWhile (<= 4000000) fibonacciSequence
  where
    fibonacciSequence =
      0 : 1 : zipWith (+) fibonacciSequence (tail fibonacciSequence)
