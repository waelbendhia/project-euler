module Level5.Problem113
  ( problem
  ) where

import Data.Array
import Data.Char

import Problem
    -- increasing numbers beginning with the digit y.

-- Some thinking first. It seems like the best approach is to count increasing
-- and decreasing numbers and subtract the sum from the total.
-- To count the number of increasing numbers under 10^n we take the following
-- approach.
-- We initialize a matrix M of size (n, 10) where the x coord signifies the
-- length of the number in digits and the y coord represents the number of
-- For M[0] we initialize the array with 1s
-- Then for each x < n and digit y M[x][y] = M[x-1][0] + ... + M[x-1][y]
-- The same principle applies more or less for decreasing numbers.
-- Then we add up both results subtract 10 first 1 digit number that'd be
-- counted twice and subtract 9 * n for numbers with a repeating digit.
problem :: Problem Integer
problem = Problem 113 "Non-bouncy numbers" (nonBouncyUnder10Exp 100)

nonBouncyUnder10Exp :: (Ix a, Num a, Enum a) => a -> a
nonBouncyUnder10Exp n = inc + dec - (n - 1) * 9 - 10
  where
    inc = increasingNumbersUnder10Exp n
    dec = decreasingNumbersUnder10Exp n

increasingNumbersUnder10Exp :: (Ix p, Num p, Num a, Enum p) => p -> a
increasingNumbersUnder10Exp n = sum $ elems $ arr ! n
  where
    arr = listArray (1, n) $ count <$> [1 .. n]
    count 1 = listArray (0, 9) $ replicate 10 1
    count n =
      listArray (0, 9) $
      fmap (\d -> sum $ fmap (arr ! (n - 1) !) [d .. 9]) [0 .. 9]

decreasingNumbersUnder10Exp :: (Ix p, Num p, Num a, Enum p) => p -> a
decreasingNumbersUnder10Exp n =
  sum $ fmap (\l -> sum $ tail $ elems $ arr ! l) [1 .. n]
  where
    arr = listArray (1, n) $ count <$> [1 .. n]
    count 1 = listArray (0, 9) $ replicate 10 1
    count n =
      listArray (0, 9) $
      fmap (\d -> sum $ fmap (arr ! (n - 1) !) [0 .. d]) [0 .. 9]
