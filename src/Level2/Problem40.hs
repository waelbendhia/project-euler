module Level2.Problem40
  ( problem
  ) where

import Data.Char

import Problem

problem :: Problem Integer
problem =
  Problem
  { ind = 40
  , name = "Champernowne's constant"
  , solution =
      toInteger $ foldl (*) 1 $ map d [1, 10, 100, 1000, 10000, 100000, 1000000]
  }

champernowneSequence :: [Int]
champernowneSequence = map digitToInt $ concat $ map show [1 ..]

d :: Int -> Int
d = (!!) champernowneSequence . (+ (-1))
