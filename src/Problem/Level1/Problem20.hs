module Problem.Level1.Problem20
  ( problem
  ) where

import Data.Char
import Problem.Problem

problem :: Problem Integer
problem =
  Problem
  { ind = 20
  , name = "Factorial digit sum"
  , solution = fromIntegral $ factDigitSum 100
  }

factDigitSum :: Integer -> Int
factDigitSum = sum . map digitToInt . show . factorial

factorial :: (Eq t, Num t) => t -> t
factorial 0 = 1
factorial n = n * factorial (n - 1)
