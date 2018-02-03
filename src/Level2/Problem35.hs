module Level2.Problem35
  ( problem
  ) where

import Problem

problem :: Problem Integer
problem =
  Problem
  { ind = 35
  , name = "Circular primes"
  , solution = toInteger $length $ circularPrimesUnder 1000000
  }

circularPrimesUnder :: Int -> [Int]
circularPrimesUnder n = filter isCircularPrime $ takeWhile (<= n) primes

isCircularPrime :: Int -> Bool
isCircularPrime = all' isPrime . loop

all' :: Foldable t => (a -> Bool) -> t a -> Bool
all' f l = length l /= 0 && all f l

containsZero :: Int -> Bool
containsZero = any ((==) '0') . show

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

isPrime :: Int -> Bool
isPrime a = isPrime' 2
  where
    isPrime' x
      | x > intSqrt a = True
      | rem a x == 0 = False
      | otherwise = isPrime' (x + 1)

intSqrt :: Int -> Int
intSqrt x = truncate $ sqrt $ (fromIntegral x :: Double)

-- Credit for this goes to wwwater from stack overflow
primes :: [Int]
primes = 2 : 3 : calcNextPrimes (tail primes) [5,7 ..]
  where
    calcNextPrimes (p:ps) candidates =
      let (smallerSquareP, (_:biggerSquareP)) = span (< p * p) candidates
      in smallerSquareP ++
         calcNextPrimes ps [c | c <- biggerSquareP, rem c p /= 0]
