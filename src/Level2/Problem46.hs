module Level2.Problem46
  ( problem
  ) where

import Data.Numbers.Primes
import Problem

problem :: Problem Integer
problem =
  Problem
  { ind = 46
  , name = "Goldbach's other conjecture"
  , solution = head $ filter (not . isGoldbachNum) oddComposites
  }

isGoldbachNum :: Integral a => a -> Bool
isGoldbachNum n =
  any (\p -> (intDiv (n - p) 2 >>= intSqrt) /= Nothing) $ takeWhile (< n) primes

oddComposites = filter (not . isPrime) [9,11 ..]

intSqrt n =
  if n < 0
    then Nothing
    else if root ^ 2 == n
           then Just root
           else Nothing
  where
    root = truncate $ sqrt $ fromIntegral n

intDiv n d
  | rem n d == 0 = Just $ div n d
  | otherwise = Nothing
