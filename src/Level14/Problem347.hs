module Level14.Problem347
  ( problem
  ) where

import Data.List
import Data.Numbers.Primes

import Problem

problem :: Problem Integer
problem =
  Problem 347 "Largest integer divisible by two primes" (capitalS (10 ^ 7))

capitalS n = helper primes
  where
    capitalM' p q = capitalM p q n
    helper [] = 0
    helper (p:ps)
      | p * p <= n =
        sum (capitalM' p <$> takeWhile ((<= n) . (* p)) ps) + helper ps
      | otherwise = 0

capitalM :: (Num p, Ord p) => p -> p -> p -> p
capitalM p q n =
  if not (null ns)
    then maximum ns
    else 0
  where
    ps = takeWhile (<= n) $ map (p ^) [1 ..]
    ns = ps >>= \p' -> takeWhile (<= n) $ (p' *) . (q ^) <$> [1 ..]
