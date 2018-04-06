module Level5.Problem123
  ( problem
  ) where

import Data.List
import Data.Maybe
import Data.Numbers.Primes
import Problem

problem :: Problem Integer
problem =
  Problem
  { ind = 123
  , name = "Prime square remainders"
  , solution =
      toInteger $ (+) 1 $ fromMaybe 0 $ findIndex (> (10 ^ 10)) fnPrimes
  }

fnPrimes :: [Integer]
fnPrimes = map (uncurry fn) $ zip primes [1 ..]

-- Using what we learned in problem 120 we can simplify our formula to this:
fn :: Integral t => t -> t -> t
fn p n
  | even n = 2
  | otherwise = 2 * n * p
