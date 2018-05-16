module Level2.Problem49
  ( problem
  ) where

import Data.List
import Data.Numbers.Primes

import Problem

problem :: Problem Integer
problem =
  Problem
    49
    "Prime permutations"
    (read $
     concatMap show $
     head $ filter (notElem 1487) $ arithmeticSharedDigitNPrimes 4)

arithmeticSharedDigitNPrimes :: (Show a, Integral a, Integral b) => b -> [[a]]
arithmeticSharedDigitNPrimes n =
  concatMap arithmeticSubsequences $ groupedBySharedDigitsNDigitalPrimes n

arithmeticSubsequences :: (Eq a, Num a) => [a] -> [[a]]
arithmeticSubsequences l =
  remConsDup $
  filter arithmeticSeries . filter ((> 2) . length) $ subsequences l

groupedBySharedDigitsNDigitalPrimes ::
     (Integral b, Integral a, Show a) => b -> [[a]]
groupedBySharedDigitsNDigitalPrimes n =
  remConsDup $
  sort $
  map sort $ filter (not . null) $ nonConsGroupBy shareDigits $ nDigitalPrimes n

arithmeticSeries :: (Num a, Eq a) => [a] -> Bool
arithmeticSeries = helper Nothing
  where
    helper d [] = True
    helper d [_] = True
    helper Nothing (x1:x2:xs) = helper (Just (x2 - x1)) (x2 : xs)
    helper (Just d) (x1:x2:xs) = d == (x2 - x1) && helper (Just d) (x2 : xs)

nDigitalPrimes :: (Integral a, Integral b) => b -> [a]
nDigitalPrimes n = takeWhile (< 10 ^ n) $ dropWhile (< 10 ^ (n - 1)) primes

shareDigits :: Show a => a -> a -> Bool
shareDigits a b = f a == f b
  where
    f = sort . show

nonConsGroupBy :: Eq t => (t -> t -> Bool) -> [t] -> [[t]]
nonConsGroupBy f l = map (\n -> filter (\m -> m /= n && f n m) l) l

remConsDup :: Eq a => [a] -> [a]
remConsDup =
  foldl
    (\p c ->
       if not (null p) && head p == c
         then p
         else c : p)
    []
