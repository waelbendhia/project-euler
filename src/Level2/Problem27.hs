module Level2.Problem27
  ( problem
  ) where

import qualified Data.Set as S
import Problem

problem :: Problem Integer
problem =
  Problem
  { ind = 27
  , name = "Quadratic primes"
  , solution =
      (\(a, b, _) -> a * b) $
      largestPair [(a, b) | a <- [-999 .. 999], b <- [-1000 .. 1000]]
  }

largestPair :: [(Integer, Integer)] -> (Integer, Integer, Int)
largestPair =
  foldl
    (\(a, b, len) (a', b') ->
       if seqLen a' b' > len
         then (a', b', seqLen a' b')
         else (a, b, len))
    (0, 0, 0)
  where
    seqLen a = length . primesProducedBy a

primesProducedBy :: Integer -> Integer -> [Integer]
primesProducedBy a b = l 0
  where
    f n = (n ^ 2) + a * n + b
    l n
      | isPrime (f n) = (f n) : l (n + 1)
      | otherwise = []

isPrime :: Integer -> Bool
isPrime 0 = False
isPrime a = testRec 2
  where
    a' = abs a
    testRec x
      | x > intSqrt a' = True
      | rem a' x == 0 = False
      | otherwise = testRec (x + 1)
    intSqrt = truncate . sqrt . fromIntegral

primesUnder :: Integer -> S.Set Integer
primesUnder n = S.fromList $ takeWhile (< n) primes

-- Credit for this goes to wwwater from stack overflow
primes :: [Integer]
primes = 2 : 3 : calcNextPrimes (tail primes) [5,7 ..]
  where
    calcNextPrimes (p:ps) candidates =
      let (smallerSquareP, (_:biggerSquareP)) = span (< p * p) candidates
      in smallerSquareP ++
         calcNextPrimes ps [c | c <- biggerSquareP, rem c p /= 0]
