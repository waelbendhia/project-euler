module Level3.Problem51
  ( problem
  ) where

import Data.Char
import Data.Numbers.Primes

import Problem

problem :: Problem Integer
problem =
  Problem
  { ind = 51
  , name = "Prime digit replacements"
  , solution = firstPrimeGeneratingSeriesOfLength 8
  }

firstPrimeGeneratingSeriesOfLength :: Int -> Integer
firstPrimeGeneratingSeriesOfLength n =
  head $ filter (any ((>= n) . length) . allPrimeReplacements) $ primes

allPrimeReplacements :: Integer -> [[Integer]]
allPrimeReplacements = map (filter isPrime) . allReplacements

allReplacements :: (Show a, Read a, Ord a) => a -> [[a]]
allReplacements n = map (flip replacements $ n) $ map digitToInt $ show n

replacements :: (Ord a, Read a, Show a) => Int -> a -> [a]
replacements d n =
  filter (>= n) $
  map read $
  map (\d' -> replaceChar (intToDigit d) d' $ show n) $ map intToDigit [0 .. 9]

replaceChar :: Eq t => t -> t -> [t] -> [t]
replaceChar _ _ [] = []
replaceChar d d' (x:xs) =
  (if x == d
     then d'
     else x) :
  replaceChar d d' xs
