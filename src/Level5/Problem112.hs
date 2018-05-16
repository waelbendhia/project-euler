module Level5.Problem112
  ( problem
  ) where

import Data.Char

import Problem

problem :: Problem Integer
problem = Problem 112 "Bouncy Number" (findPercentageOfBouncy 99)

isIncreasing :: (Show a, Integral a) => a -> Bool
isIncreasing = helper . map digitToInt . show
  where
    helper l =
      case l of
        [] -> True
        [_] -> True
        x1:x2:xs -> (x1 <= x2) && helper (x2 : xs)

isDecreasing :: (Show a, Integral a) => a -> Bool
isDecreasing = helper . map digitToInt . show
  where
    helper l =
      case l of
        [] -> True
        [_] -> True
        x1:x2:xs -> (x1 >= x2) && helper (x2 : xs)

isBouncy :: (Integral a, Show a) => a -> Bool
isBouncy num = not (isDecreasing num || isIncreasing num)

findPercentageOfBouncy :: (Integral t, Integral a, Show a) => t -> a
findPercentageOfBouncy pct = helper 1 0 0
  where
    checkPct bouncy total =
      total > 0 && fromIntegral pct * (fromIntegral total / 100) <= bouncy
    helper x bouncy total
      | checkPct bouncy total = x - 1
      | otherwise =
        helper
          (x + 1)
          (bouncy +
           if isBouncy x
             then 1
             else 0)
          (total + 1)
