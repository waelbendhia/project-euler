module Level1.Problem12
  ( problem
  ) where

import Problem

problem :: Problem Integer
problem = Problem 12 "Highly divisible triangular number" (solver 500)

solver :: Int -> Integer
solver minimumFactors =
  head $ dropWhile ((<= minimumFactors) . numFactors) triangleNumbers

wholeRoot :: Integer -> Integer
wholeRoot x = truncate $ sqrt $ fromIntegral x

triangleNumbers :: [Integer]
triangleNumbers = map (\x -> sum [1 .. x]) [1 ..]

numFactors :: Integer -> Int
numFactors x = 2 + (2 * length ftors)
  where
    ftors = [y | y <- [2 .. (wholeRoot x)], rem x y == 0]
