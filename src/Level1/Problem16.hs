module Level1.Problem16
  ( problem
  ) where

import Data.Char
import Problem

problem :: Problem Integer
problem = Problem 16 "Power digit sum" (sumDigits (2 ^ 1000))

sumDigits :: Integer -> Integer
sumDigits = toInteger . sum . map digitToInt . show
