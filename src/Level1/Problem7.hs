module Level1.Problem7
  ( problem
  ) where

import Data.Numbers.Primes
import Problem

problem :: Problem Integer
problem = Problem {ind = 7, name = "10001st prime", solution = primes !! 10000}
