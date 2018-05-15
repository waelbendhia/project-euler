module Level1.Problem10
  ( problem
  ) where

import Data.Numbers.Primes
import Problem

problem :: Problem Integer
problem =
  Problem 10 "Summation of primes" (sum $ takeWhile (< 2000000) $ primes)
