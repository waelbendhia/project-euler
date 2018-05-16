module Level2.Problem48
  ( problem
  ) where

import Problem

problem :: Problem Integer
problem =
  Problem 48 "Self powers" (lastNDigits 10 $ sum $ take 1000 powerSeries)

lastNDigits :: Int -> Integer -> Integer
lastNDigits n = read . reverse . take n . reverse . show

powerSeries :: [Integer]
powerSeries = map (\n -> n ^ n) [1 ..]
