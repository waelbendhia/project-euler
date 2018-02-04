module Level1.Problem10
  ( problem
  ) where

import Data.Numbers.Primes
import Problem

problem :: Problem Integer
problem =
  Problem
  { ind = 10
  , name = "Summation of primes"
  , solution = sum $ takeWhile (< 2000000) $ primes
  }
