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
     head $ filter (not . any (== 1487)) $ arithmeticSharedDigitNPrimes 4)

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
  map sort $
  filter ((> 0) . length) $ nonConsGroupBy shareDigits $ nDigitalPrimes n

arithmeticSeries :: (Num a, Eq a) => [a] -> Bool
arithmeticSeries l = helper Nothing l
  where
    helper d [] = True
    helper d [_] = True
    helper (Nothing) (x1:x2:xs) = helper (Just (x2 - x1)) (x2 : xs)
    helper (Just d) (x1:x2:xs) =
      if d == (x2 - x1)
        then helper (Just d) (x2 : xs)
        else False

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
       if length p > 0 && head p == c
         then p
         else c : p)
    []
