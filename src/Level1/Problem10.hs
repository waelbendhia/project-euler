module Problem10
  ( problem10
  ) where

import Problem

problem10 :: Problem Integer
problem10 =
  Problem {ind = 10, name = "Summation of primes", solution = fromIntegral sol}

sol :: Integer
sol = sum $ takeWhile (< 2000000) $ primes

-- Credit for this goes to wwwater from stack overflow
primes :: [Integer]
primes = 2 : 3 : calcNextPrimes (tail primes) [5,7 ..]
  where
    calcNextPrimes (p:ps) candidates =
      let (smallerSquareP, (_:biggerSquareP)) = span (< p * p) candidates
      in smallerSquareP ++
         calcNextPrimes ps [c | c <- biggerSquareP, rem c p /= 0]
