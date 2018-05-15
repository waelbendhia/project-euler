module Level6.Problem145
  ( problem
  ) where

import Problem

problem :: Problem Integer
problem =
  Problem
    145
    "How many reversible numbers are there below one-billion?"
    (numReversibleWithNDigits 9)

-- As a base we can see for two digits numbers we have 20 solutions.
-- For numbers with even number of digits we have:
-- 20 possible solutions the outer pair and 30 possible solutions for every 
-- inner pair.
-- I'll finish writing this latter
numReversibleWithNDigits :: (Integral a1, Num a) => a1 -> a
numReversibleWithNDigits n
  | n <= 1 = 0
  | even n = (20 * 30 ^ (n `div` 2 - 1)) + numReversibleWithNDigits (n - 1)
  | n `rem` 4 == 1 =
    (100 * 500 ^ (max 0 $ n `div` 4 - 1)) + numReversibleWithNDigits (n - 1)
  | otherwise = numReversibleWithNDigits (n - 1)
