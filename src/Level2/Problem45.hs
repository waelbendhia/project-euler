module Level2.Problem45
  ( problem
  ) where

import Problem

problem :: Problem Integer
problem =
  Problem
  { ind = 45
  , name = "Triangular, pentagonal, and hexagonal"
  , solution = head $ dropWhile (<= 40755) triHexPentTerms
  }

triHexPentTerms :: [Integer]
triHexPentTerms =
  filter ((/= Nothing) . pentagonalTerm) $
  filter ((/= Nothing) . hexagonTerm) $ map (\n -> div (n * (n + 1)) 2) [1 ..]

hexagonTerm :: Integer -> Maybe Integer
hexagonTerm n =
  discriminant >>=
  (\d ->
     Just $
     if d + 1 > 0
       then 1 + d
       else 1 - d) >>= \x ->
    if rem x 4 == 0
      then Just $ div x 4
      else Nothing
  where
    discriminant = intSqrt (1 + 8 * n)

triangleTerm :: Integer -> Maybe Integer
triangleTerm n =
  discriminant >>=
  (\d ->
     Just $
     if d - 1 > 0
       then d - 1
       else -1 - d) >>= \v ->
    if even v
      then Just $ v `div` 2
      else Nothing
  where
    discriminant = intSqrt (1 + 8 * n)

pentagonalTerm :: Integer -> Maybe Integer
pentagonalTerm p =
  discriminant >>= \d ->
    Just
      (if 1 + d > 0
         then 1 + d
         else 1 - d) >>= \x ->
      if rem x 6 == 0
        then Just (div x 6)
        else Nothing
  where
    discriminant = intSqrt (1 + 24 * p)

intSqrt :: Integer -> Maybe Integer
intSqrt n =
  if n < 0
    then Nothing
    else if root ^ 2 == n
           then Just root
           else Nothing
  where
    root = truncate $ sqrt $ fromIntegral n
