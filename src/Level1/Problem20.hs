module Level1.Problem20
  ( problem
  ) where

import Data.Char
import Problem

problem :: Problem Integer
problem = Problem 20 "Factorial digit sum" (fromIntegral $ factDigitSum 100)

factDigitSum :: Integer -> Int
factDigitSum = sum . map digitToInt . show . factorial

factorial :: (Enum a, Num a) => a -> a
factorial = product . enumFromTo 1
