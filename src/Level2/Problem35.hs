module Level2.Problem35
  ( problem
  ) where

import Data.Numbers.Primes
import Problem

problem :: Problem Integer
problem =
  Problem 35 "Circular primes" (toInteger $length $ circularPrimesUnder 1000000)

circularPrimesUnder :: Int -> [Int]
circularPrimesUnder n = filter isCircularPrime $ takeWhile (<= n) primes

isCircularPrime :: Int -> Bool
isCircularPrime = all' isPrime . loop

all' :: Foldable t => (a -> Bool) -> t a -> Bool
all' f l = not (null l) && all f l

containsZero :: Int -> Bool
containsZero = elem '0' . show

loop :: Int -> [Int]
loop n =
  if containsZero n
    then []
    else loop' n []
  where
    loop' x [] = loop' x [x]
    loop' x l@(x':_) =
      if shift x' == x
        then l
        else loop' x (shift x' : l)

shift :: Int -> Int
shift n = read $ last sn : init sn
  where
    sn = show n
