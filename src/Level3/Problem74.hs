module Level3.Problem74
  ( problem
  ) where

import Data.Char
import qualified Data.IntMap as M

import Problem

problem :: Problem Integer
problem =
  Problem
    74
    "Digit factorial chains"
    (toInteger $
     length $
     M.filter (== 60) $
     M.filterWithKey (\k _ -> k <= 1000000) $ allFactorialLengths 1000000)

allFactorialLengths :: Num a => M.Key -> M.IntMap a
allFactorialLengths n =
  foldl factorialLength (M.fromList [(169, 3), (871, 2), (872, 2)]) [1 .. n]

factorialLength :: Num a => M.IntMap a -> M.Key -> M.IntMap a
factorialLength m n =
  case M.lookup n m of
    Just _ -> m
    Nothing ->
      if digitSumFactorial n == n
        then M.insert n 1 m
        else M.insert n (1 + nextCount) nextM
  where
    nextM = factorialLength m (digitSumFactorial n)
    nextCount = nextM M.! digitSumFactorial n

digitSumFactorial :: Show a => a -> Int
digitSumFactorial = sum . map (factorial . digitToInt) . show

factorial :: (Enum a, Num a) => a -> a
factorial = product . enumFromTo 1
