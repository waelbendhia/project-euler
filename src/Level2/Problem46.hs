module Level2.Problem46
  ( problem
  ) where

import Data.Numbers.Primes
import Problem

problem :: Problem Integer
problem =
  Problem
    46
    "Goldbach's other conjecture"
    (head $ filter (not . isGoldbachNum) oddComposites)

isGoldbachNum :: Integral a => a -> Bool
isGoldbachNum n =
  any (\p -> (intDiv (n - p) 2 >>= intSqrt) /= Nothing) $ takeWhile (< n) primes

oddComposites :: [Integer]
oddComposites = filter (not . isPrime) [9,11 ..]

intSqrt :: Integral a => a -> Maybe a
intSqrt n =
  if n < 0
    then Nothing
    else if root ^ 2 == n
           then Just root
           else Nothing
  where
    root = truncate $ sqrt $ fromIntegral n

intDiv :: Integral a => a -> a -> Maybe a
intDiv n d
  | rem n d == 0 = Just $ div n d
  | otherwise = Nothing
