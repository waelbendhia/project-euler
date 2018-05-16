module Level2.Problem36
  ( problem
  ) where

import Data.Char
import Problem

problem :: Problem Integer
problem =
  Problem
    36
    "Double-base palindromes"
    (toInteger $ sum $ doubleBasePalindromicUnder 1000000)

doubleBasePalindromicUnder :: Int -> [Int]
doubleBasePalindromicUnder n =
  filter (isPalindrome . toBinary) $ filter isPalindromeNumber [1 .. n]

isPalindromeNumber :: Show a => a -> Bool
isPalindromeNumber = isPalindrome . show

isPalindrome :: Eq a => [a] -> Bool
isPalindrome x =
  length x <= 1 || (last x == head x && isPalindrome (tail $ init x))

toBinary :: Int -> String
toBinary 0 = []
toBinary n = toBinary (quot n 2) ++ [intToDigit $ rem n 2]
