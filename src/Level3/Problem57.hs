module Level3.Problem57
  ( problem
  ) where

import Problem

problem :: Problem Integer
problem =
  Problem
  { ind = 57
  , name = "Square root convergents"
  , solution =
      toInteger $
      length $ filter numeratorLonger $ map approximateSquareRoot2 [1 .. 1000]
  }

-- The approximation is the series u(n+1) = 1 + 1 / (1 + u(n)) and u(0) = 1.
-- Using this definition we can determine that numerator for u(n+1) is 
-- N(n+1) = 2*D(n) + N(n) and the denominator is D(n+1) = D(n) + N(n)
numeratorLonger :: (Integer, Integer) -> Bool
numeratorLonger (n, d) = numDigits n > numDigits d

numDigits :: Integer -> Int
numDigits = length . show

approximateSquareRoot2 :: (Ord a, Integral t, Num a) => a -> (t, t)
approximateSquareRoot2 n
  | n < 1 = (1, 1)
  | otherwise = simplify (2 * nextD + nextN, nextN + nextD)
  where
    (nextN, nextD) = approximateSquareRoot2 (n - 1)

simplify :: Integral t => (t, t) -> (t, t)
simplify (a, b) = (simp' a, simp' b)
  where
    simp' n = div n $ gcd a b
