module Level2.Problem36
  ( problem
  ) where

import Data.Char
import Problem

problem :: Problem Integer
problem =
  Problem
  { ind = 36
  , name = "Double-base palindromes"
  , solution = toInteger $ sum $ doubleBasePalindromicUnder 1000000
  }

doubleBasePalindromicUnder :: Int -> [Int]
doubleBasePalindromicUnder n =
  filter (isPalindrome . toBinary) $ filter isPalindromeNumber [1 .. n]

isPalindromeNumber :: Show a => a -> Bool
isPalindromeNumber = isPalindrome . show

isPalindrome :: Eq a => [a] -> Bool
isPalindrome x =
  if length x <= 1
    then True
    else last x == head x && isPalindrome (tail $ init x)

toBinary :: Int -> [Char]
toBinary 0 = []
toBinary n = toBinary (quot n 2) ++ [intToDigit $ rem n 2]
