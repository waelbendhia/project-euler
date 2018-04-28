module Level13.Problem301
  ( problem
  ) where

import Data.Bits

import Problem

problem :: Problem Integer
problem =
  Problem
  {ind = 301, name = "Nim", solution = toInteger $ result $ (2 ^ 30 :: Int)}

capitalX :: Bits a => a -> a -> a -> a
capitalX n1 n2 n3 = n1 `xor` n2 `xor` n3

result :: (Enum a, Bits a, Num a) => a -> Int
result b = length $ filter ((== 0) . eval) [1 .. b]
  where
    eval n = capitalX n (2 * n) (3 * n)
