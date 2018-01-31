module Problem.Level1.Problem10
  ( problem
  ) where

import Problem.Problem

problem :: Problem Integer
problem =
  Problem {ind = 10, name = "Summation of primes", solution = solver 2000000}

solver :: Integer -> Integer
solver top = sum $ takeWhile (< top) $ primes

-- Credit for this goes to wwwater from stack overflow
primes :: [Integer]
primes = 2 : 3 : calcNextPrimes (tail primes) [5,7 ..]
  where
    calcNextPrimes (p:ps) candidates =
      let (smallerSquareP, (_:biggerSquareP)) = span (< p * p) candidates
      in smallerSquareP ++
         calcNextPrimes ps [c | c <- biggerSquareP, rem c p /= 0]
