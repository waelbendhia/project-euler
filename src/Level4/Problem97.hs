module Level4.Problem97
  ( problem
  ) where

import Problem

problem :: Problem Integer
problem =
  Problem 97 "Large non-Mersenne prime" (largeNonMersennePrime `mod` 10 ^ 10)

largeNonMersennePrime :: Integer
largeNonMersennePrime = 28433 * (2 ^ 7830457) + 1
