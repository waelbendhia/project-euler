module Level1.Problem24
  ( problem
  ) where

import Problem

problem :: Problem Integer
problem =
  Problem
  { ind = 24
  , name = "Lexicographic permutations"
  , solution = read $ findNthPermutation (1000000 - 1) "0123456789"
  }

findNthPermutationIndexAt1 :: Eq a => Int -> [a] -> [a]
findNthPermutationIndexAt1 n s = findNthPermutation (n - 1) s

findNthPermutation :: Eq a => Int -> [a] -> [a]
findNthPermutation n s =
  if length s <= 1
    then s
    else (s !! steps) :
         findNthPermutation remainder (filter (/= (s !! steps)) s)
  where
    step = factorial $ (length s) - 1
    steps = quot n step
    remainder = mod n step

factorial :: Integral t => t -> t
factorial n
  | n <= 1 = 1
  | otherwise = n * factorial (n - 1)
