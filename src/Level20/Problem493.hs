module Level20.Problem493
  ( problem
  ) where

import Problem

problem :: Problem Integer
problem = Problem 493 "Under The Rainbow" (round $ (* 10 ^ 9) $ solve 7 10 20)

solve :: Fractional b => Integer -> Integer -> Integer -> b
solve numColors ballsPerColor n =
  let numBalls = numColors * ballsPerColor -- Total number of balls
    -- In n picks playouts how many playouts we end up not picking a specific
    -- color
      notPicked = binomialCoefficient (numBalls - ballsPerColor) n
    -- In n picks how many playouts we have totals
      total = binomialCoefficient numBalls n
    -- Probability of not picking a specific color
      notPickedP = fromIntegral notPicked / fromIntegral total
    -- Probability of picking a color
      pickedP = 1 - notPickedP
      -- Multiply by number of colors and we get the expected number of
      -- distinct colors.
   in (fromIntegral numColors) * pickedP

binomialCoefficient :: Integer -> Integer -> Integer
binomialCoefficient n k = (product $ map f [1 .. k]) `quot` (product [1 .. k])
  where
    f i = n + 1 - i
