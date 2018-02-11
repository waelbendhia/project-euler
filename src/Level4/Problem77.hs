module Level4.Problem77
  ( problem
  ) where

import qualified Data.IntMap as M
import Data.Numbers.Primes

import Problem

problem :: Problem Integer
problem =
  Problem
  { ind = 77
  , name = "Prime summations"
  , solution =
      toInteger $ head $ dropWhile ((< 5000) . primeSummationsFor) [1 ..]
  }

-- This problem resembles problem 76 except it's primes instead of integers
primeSummationsFor :: Num a => Int -> a
primeSummationsFor val =
  (if isPrime val
     then 0
     else -1) +
  helper (takeWhile (< val) primes) 0 (M.singleton 0 1) !? val
  where
    (!?) m k =
      if M.member k m
        then m M.! k
        else 0
    helper [] _ memo = memo
    helper (n:ns) v memo =
      if v > val
        then helper (ns) 0 memo
        else helper (n : ns) (v + 1) $ M.insertWith (+) v (memo !? (v - n)) memo
