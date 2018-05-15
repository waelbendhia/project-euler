module Level3.Problem52
  ( problem
  ) where

import Data.List
import Problem

problem :: Problem Integer
problem =
  Problem
    52
    "Permuted multiples"
    (head $ filter (\n -> all (shareDigits n) $ multiples n) [1 ..])

multiples :: (Enum b, Num b) => b -> [b]
multiples n = map (* n) [2 .. 6]

shareDigits :: Show a => a -> a -> Bool
shareDigits a b = f a == f b
  where
    f = sort . show
