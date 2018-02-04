module Level2.Problem27
  ( problem
  ) where

import Data.Numbers.Primes
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
