module Level5.Problem119
  ( problem
  ) where

import Data.Char
import Data.List

import Problem

-- Hacky but it works
problem :: Problem Integer
problem =
  Problem
    119
    "Digit power sum"
    (filter isPowerDigitSum (allPowersBounded 100) !! 29)

sumDigits :: (Show a, Integral a) => a -> a
sumDigits = fromIntegral . sum . map digitToInt . show

isPowerOf :: (Num t, Ord t) => t -> t -> Bool
isPowerOf n x = helper 1
  where
    helper p
      | n ^ p == x = True
      | n ^ p > x = False
      | otherwise = helper (p + 1)

digitPowerSums :: [Integer]
digitPowerSums =
  filter (\x -> sumDigits x /= 1 && isPowerOf (sumDigits x) x) [10 ..]

isPowerDigitSum :: (Integral t, Show t) => t -> Bool
isPowerDigitSum x = sumDigits x /= 1 && isPowerOf (sumDigits x) x

allPowersBounded :: Integral b => b -> [b]
allPowersBounded b =
  map head $ group $ sort [n ^ p | n <- [2 .. b], p <- [2 .. b], n ^ p > 10]
