module Problem.Level1.Problem16
  ( problem
  ) where

import Data.Char
import Problem.Problem

problem :: Problem Integer
problem =
  Problem {ind = 16, name = "Power digit sum", solution = sumDigits (2 ^ 1000)}

sumDigits :: Integer -> Integer
sumDigits = toInteger . sum . map digitToInt . show
