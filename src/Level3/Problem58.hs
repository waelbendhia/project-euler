module Level3.Problem58
  ( problem
  ) where

import Data.Numbers.Primes

import Problem

problem :: Problem Integer
problem = Problem 58 "Spiral primes" (toInteger $ sideLengthForRatioUnder 1 10)

sideLengthForRatioUnder :: Int -> Int -> Int
sideLengthForRatioUnder n d = div (minLength - 1) 2 + 1
  where
    minLength = helper 0 1 $ tail spiralDiagonals
    helper ps ns (x1:x2:x3:x4:xs) =
      if ns > 1 && ps * d < ns * n
        then ns
        else helper
               (ps + (length $ filter isPrime [x1, x2, x3, x4]))
               (ns + 4)
               xs

spiralDiagonals :: [Integer]
spiralDiagonals = scanl (+) 1 $ concatMap (replicate 4) $ map (* 2) [1 ..]
