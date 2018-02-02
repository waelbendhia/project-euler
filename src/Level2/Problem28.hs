module Level2.Problem28
  ( problem
  ) where

import Problem

problem :: Problem Integer
problem =
  Problem
  { ind = 28
  , name = "Number spiral diagonals"
  , solution = sum $ spiralDiagonals 1001
  }

spiralDiagonals :: Integer -> [Integer]
spiralDiagonals n = spiralDiag [1]
  where
    spiralDiag l
      | step > n = l
      | otherwise = spiralDiag (nextTerm : l)
      where
        nextTerm = calcNextTerm l
        step = (ceiling ((toRational $ length l) / 4)) * 2
        calcNextTerm l = step + head l
