{-# LANGUAGE LambdaCase #-}

module Level20.Problem491
  ( problem
  ) where

import Data.List

import Problem

-- https://en.wikipedia.org/wiki/Divisibility_rule
-- After much consideration the rule:
-- Form the alternating sum of the digits. The result must be divisible by 11.
-- Seems to be the most fruitful one.
-- Let Se be the sum of even numbered digits, So be the sum of odd numbered
-- digits and S be the sum of all digits in:
-- So + Se = S
-- So - Se = 0 (mod 11)
-- So - (S - So) = 0 (mod 11)
-- 2.So - S = 0 (mod 11)
-- So first we need to count then number of ways where you can split the set in
-- half where that validates that rule.
-- For each set we theoretically have 10! combinations, except we can't count
-- duplicate digits twice.
-- So the final formula for each half of the set is:
-- 10! / 2^n
-- where n is how many duplicate digits we have. Also one tenth of these will 
-- start with 0. So we need to remove those frome the total.
problem :: Problem Integer
problem =
  Problem
    491
    "Double pandigital number divisible by 11"
    (toInteger $
     ((* 9) . (`div` 10)) $
     sum $
     map permutationsOfPosition $ filter validPosition $ generatePositions 10 0)

permutationsOfPosition l =
  tenF ^ 2 `div`
  (product (filter (== 2) l) * product (filter (== 2) secondHalf))
  where
    secondHalf = zipWith (-) (replicate 10 2) l
    tenF = product [1 .. 10]

validPosition l =
  sum l == 10 &&
  all (<= 2) l &&
  ((sum (zipWith (*) l [0 .. 9]) - 45) `mod` 11) == 0 && sum l == 10

generatePositions rem len
  | len == 9 = [[rem]]
  | rem == 0 = [replicate (10 - len) 0]
  | otherwise =
    concat $
    (\n -> (n :) <$> generatePositions (rem - n) (len + 1)) <$> [0 .. min 2 rem]
