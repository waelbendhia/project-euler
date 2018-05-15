module Level2.Problem50
  ( problem
  ) where

import Data.List
import Data.Maybe
import Data.Numbers.Primes
import qualified Data.Vector as V

import Problem

problem :: Problem Integer
problem = Problem 50 "Consecutive prime sum" (longestPrimeSumUnder 1000000)

longestPrimeSumUnder :: Integral t => t -> t
longestPrimeSumUnder n =
  fromMaybe 0 $ fmap sumByInd $ find eval $ filter eval indexPairs
  where
    eval (a, b) = sumByInd (a, b) < n && (isPrime $ sumByInd (a, b))
    sumByInd (a, b) = (sumP' V.! b - sumP' V.! a)
    indexPairs = do
      r <- [length sumP',length sumP' - 1 .. 2]
      map (\i -> (i, i + r)) [0 .. length sumP' - r - 1]
    sumP' = V.fromList $ tail $ scanl (+) 0 $ takeWhile (< n) primes
