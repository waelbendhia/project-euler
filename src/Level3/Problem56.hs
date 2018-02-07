module Level3.Problem56
  ( problem
  ) where

import Data.Char
import Problem

problem :: Problem Integer
problem =
  Problem
  { ind = 56
  , name = "Powerful digit sum"
  , solution = maximum $ map powerTupleDigitSum $ allPairsUnderN 100
  }

powerTupleDigitSum :: (Integer, Integer) -> Integer
powerTupleDigitSum = digitSum . uncurry (^)

digitSum :: Integer -> Integer
digitSum = sum . map (toInteger . digitToInt) . show

allPairsUnderN :: (Enum t, Num t) => t -> [(t, t)]
allPairsUnderN n = [(a, b) | a <- [1 .. n - 1], b <- [1 .. n - 1]]
