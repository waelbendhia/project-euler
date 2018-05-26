module Level21.Problem516
  ( problem
  ) where

import Control.Monad.ST
import Data.Array
import Data.Array.MArray
import Data.Array.ST
import Data.Foldable
import Math.NumberTheory.Primes (primes)

-- import Data.Numbers.Primes (primes)
import Problem

problem :: Problem Integer
problem = Problem 516 "5-smooth totients" (fromIntegral $ sumTriplets $ 10 ^ 5)

sumTriplet (a, b, c) = a + b + c

triplets limit = helper primesList
  where
    primesList = takeWhile (< limit - 2) $ map fromIntegral primes
    isPrime = createPrimeLookup limit primesList
    helper [] = []
    helper (a:ps) =
      (do b <- takeWhile ((< limit) . nextGeomtric a) ps
          c <- hasGeometric isPrime a b
          return (a, b, c)) ++
      helper ps

sumTriplets limit = helper primesList
  where
    primesList = takeWhile (< limit - 2) $ map fromIntegral primes
    isPrime = createPrimeLookup limit primesList
    helper [] = 0
    helper (a:ps) =
      sum
        (do b <- takeWhile ((< limit) . nextGeomtric a) ps
            c <- hasGeometric isPrime a b
            return $ a + b + c) +
      helper ps

hasGeometric isPrime a b = [c | isPrime c && cRem == 0]
  where
    c = nextGeomtric a b
    cRem = ((b + 1) ^ 2) `mod` (a + 1)

nextGeomtric a b = (((b + 1) ^ 2) `div` (a + 1)) - 1

createPrimeLookup :: Int -> [Int] -> Int -> Bool
createPrimeLookup b l = (lookup !)
  where
    lookup =
      runSTArray $ do
        arr <- newArray (1, b + 4) False :: ST s (STArray s Int Bool)
        forM_ l $ flip (writeArray arr) True
        return arr
