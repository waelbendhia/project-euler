module Problem7
  ( problem7
  ) where

import Problem

problem7 :: Problem Integer
problem7 = Problem {ind = 7, name = "10001st prime", solution = sol}

sol :: Integer
sol = primes !! 10000

-- Credit for this goes to wwwater from stack overflow
primes :: [Integer]
primes = 2 : 3 : calcNextPrimes (tail primes) [5,7 ..]
  where
    calcNextPrimes (p:ps) candidates =
      let (smallerSquareP, (_:biggerSquareP)) = span (< p * p) candidates
      in smallerSquareP ++
         calcNextPrimes ps [c | c <- biggerSquareP, rem c p /= 0]
