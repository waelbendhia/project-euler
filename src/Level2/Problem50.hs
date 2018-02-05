module Level2.Problem50
  ( problem
  ) where

import Data.List
import Data.Numbers.Primes
import qualified Data.Vector as V

import Problem

problem :: Problem Integer
problem =
  Problem
  { ind = 50
  , name = "Consecutive prime sum"
  , solution = longestPrimeSumUnder 1000000
  }

longestPrimeSumUnder :: Integral t => t -> t
longestPrimeSumUnder n =
  case (find eval $ filter eval indexPairs) >>= Just . sumByInd of
    Nothing -> 0
    Just x -> x
  where
    eval (a, b) = sumByInd (a, b) < n && (isPrime $ sumByInd (a, b))
    sumByInd (a, b) = (sumP' V.! b - sumP' V.! a)
    indexPairs =
      concatMap
        (\r -> map (\i -> (i, i + r)) [0 .. length sumP' - r - 1])
        [length sumP',length sumP' - 1 .. 2]
    sumP' = V.fromList $ tail $ scanl (+) 0 $ takeWhile (< n) primes
