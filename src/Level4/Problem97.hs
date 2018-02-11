module Level4.Problem97
  ( problem
  ) where

import Problem

problem :: Problem Integer
problem =
  Problem
  { ind = 97
  , name = "Large non-Mersenne prime"
  , solution = largeNonMersennePrime `mod` 10 ^ 10
  }

largeNonMersennePrime :: Integer
largeNonMersennePrime = 28433 * (2 ^ 7830457) + 1
