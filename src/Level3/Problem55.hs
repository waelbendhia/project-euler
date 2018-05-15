module Level3.Problem55
  ( problem
  ) where

import Problem

problem :: Problem Integer
problem =
  Problem
    55
    "Lychrel numbers"
    (toInteger $ length $ filter lychrel [1 .. 10000])

lychrel :: (Read a1, Show a1, Num a1) => a1 -> Bool
lychrel n = helper 0 n
  where
    helper 50 _ = True
    helper 0 x = helper 1 (lychrelNext x)
    helper i x =
      if isPalindromeNumber x
        then False
        else helper (i + 1) (lychrelNext x)

isPalindromeNumber :: Show a => a -> Bool
isPalindromeNumber = isPalindrome . show

isPalindrome :: Eq a => [a] -> Bool
isPalindrome x =
  if length x <= 1
    then True
    else last x == head x && isPalindrome (tail $ init x)

flipNum :: (Show a1, Read a) => a1 -> a
flipNum = read . reverse . show

lychrelNext :: (Read a1, Show a1, Num a1) => a1 -> a1
lychrelNext n = n + flipNum n
