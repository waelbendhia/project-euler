module Level1.Problem4
  ( problem
  ) where

import Data.List
import Problem

problem :: Problem Integer
problem =
  Problem {ind = 4, name = "Largest palindrome product", solution = solver 3}

solver :: Int -> Integer
solver digits = head $ dropWhile (not . isPalindrome . show) allFactors
  where
    bound = (10 ^ digits) - 1
    isPalindrome x =
      if length x <= 1
        then True
        else last x == head x && isPalindrome (tail $ init x)
    allFactors =
      sortBy (flip compare) $
      concatMap (\x -> map (* x) [100 .. x]) [100 .. bound]
