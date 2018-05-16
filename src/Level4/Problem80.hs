module Level4.Problem80
  ( problem
  ) where

import Data.Char

import Problem

problem :: Problem Integer
problem =
  Problem
    80
    "Square root digital expansion"
    (toInteger $
     sum $ map (sumNDigitsRoot 100) $ filter (not . isSquare) [1 .. 100])

sumNDigitsRoot :: Integer -> Integer -> Int
sumNDigitsRoot = ((sum . map digitToInt) .) . nDigitsRoot

nDigitsRoot :: (Integral b, Integral a, Show a) => b -> a -> String
nDigitsRoot n = take 100 . show . newtonRoot . (* 10 ^ (2 * n))

newtonRoot :: Integral a => a -> a
newtonRoot n = helper n
  where
    helper x =
      if x ^ 2 > n
        then helper $ (x + n `div` x) `div` 2
        else x

wholeRoot :: Integer -> Integer
wholeRoot = truncate . sqrt . fromInteger

isSquare :: Integer -> Bool
isSquare x = wholeRoot x ^ 2 == x
