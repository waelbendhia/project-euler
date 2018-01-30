module Problem4
  ( problem4
  ) where

import Data.List
import Problem

problem4 :: Problem Integer
problem4 =
  Problem {ind = 4, name = "Largest palindrome product", solution = sol}

sol :: Integer
sol = head $ dropWhile (not . isPalindrome . show) allFactors
  where
    isPalindrome x =
      if length x <= 1
        then True
        else last x == head x && isPalindrome (tail $ init x)
    allFactors =
      sortBy (flip compare) $
      concatMap (\x -> map (* x) [100 .. x]) [100 .. 999]
