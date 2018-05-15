module Level5.Problem125
  ( problem
  ) where

import Data.List

import Problem

-- The first approach I tried was to filter all numbers first if they're
-- palindromes then if they're square sums. However this proved too slow also
-- I'm too lazy to write a function that checks if a number is a sum of squares
-- that runs in linear time. Even then the algorithm would run in quadratic
-- time which is about the same time complexity (if you ignore the sorting and
-- grouping) as generating all square sums and checking if they're palindrome
-- hence this solution.
problem :: Problem Integer
problem =
  Problem
    125
    "Palindromic sums"
    (sum $ filter isPalindromeNumber $ allSquareSumsUnder $ 10 ^ 8)

isPalindromeNumber :: (Integral a, Show a) => a -> Bool
isPalindromeNumber = isPalindrome . show

isPalindrome :: Eq a => [a] -> Bool
isPalindrome x
  | length x <= 1 = True
  | otherwise = last x == head x && isPalindrome (tail $ init x)

isSumOfConsecutiveSquares :: (Ord t, Enum t, Num t) => t -> Bool
isSumOfConsecutiveSquares x = helper $ map (^ 2) [1 ..]
  where
    sumFromStart (s:ss) total
      | total == x = True
      | total > x = False
      | otherwise = sumFromStart ss (total + s)
    helper (s:ss)
      | s >= x = False
      | sumFromStart ss s = True
      | otherwise = helper ss

intSqrt :: Integer -> Integer
intSqrt = truncate . sqrt . fromIntegral

allSquareSumsUnder :: Integer -> [Integer]
allSquareSumsUnder bound =
  map head $
  group $
  sort $
  [1 .. intSqrt bound] >>= \x ->
    filter (< bound) $ drop 2 $ scanl (+) 0 $ map (^ 2) [x - 1,x - 2 .. 1]
