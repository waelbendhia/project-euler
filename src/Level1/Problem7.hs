module Level1.Problem7
  ( problem
  ) where

import Problem

problem :: Problem Integer
problem = Problem {ind = 7, name = "10001st prime", solution = solver 10001}

solver :: Int -> Integer
solver n = primes !! (n - 1)

-- Credit for this goes to wwwater from stack overflow
primes :: [Integer]
primes = 2 : 3 : calcNextPrimes (tail primes) [5,7 ..]
  where
    calcNextPrimes (p:ps) candidates =
      let (smallerSquareP, (_:biggerSquareP)) = span (< p * p) candidates
      in smallerSquareP ++
         calcNextPrimes ps [c | c <- biggerSquareP, rem c p /= 0]
