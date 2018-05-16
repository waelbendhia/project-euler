module Level1.Problem4
  ( problem
  ) where

import Data.List
import Problem

problem :: Problem Integer
problem = Problem 4 "Largest palindrome product" (solver 3)

solver :: Int -> Integer
solver digits = head $ dropWhile (not . isPalindrome . show) allFactors
  where
    bound = (10 ^ digits) - 1
    isPalindrome x =
      length x <= 1 || (last x == head x && isPalindrome (tail $ init x))
    allFactors =
      sortBy (flip compare) $
      concatMap (\x -> map (* x) [100 .. x]) [100 .. bound]
