module Level2.Problem38
  ( problem
  ) where

import Data.Char
import qualified Data.Set as S
import Problem

problem :: Problem Integer
problem =
  Problem
    38
    "Pandigital mutliples"
    (maximum $ map producedNumber $ filter producesPandigitalSeries [2 .. 9876])

multiplesUntilLength :: Integer -> [Integer]
multiplesUntilLength x = take' [] $ map (* x) [1 ..]
  where
    take' l l'
      | (sum $ map numDigits l) == 9 = l
      | (sum $ map numDigits l) > 9 = []
      | otherwise = take' (l ++ [head l']) (tail l')

producedNumber :: Integer -> Integer
producedNumber = seriesNum . multiplesUntilLength

seriesNum :: [Integer] -> Integer
seriesNum = read . concat . map show

numDigits :: Integer -> Integer
numDigits = (+ 1) . truncate . logBase 10 . fromIntegral

isPandigitalSeries :: [Integer] -> Bool
isPandigitalSeries = isPandigital . seriesNum

isPandigital :: Integer -> Bool
isPandigital n = (9 == numDigits n) && (not . containsRepeatingDigits) n

containsRepeatingDigits :: Show a => a -> Bool
containsRepeatingDigits =
  (== 0) . S.size . foldl folder (S.singleton 0) . map digitToInt . show
  where
    folder p c
      | S.size p == 0 = S.empty
      | S.member c p = S.empty
      | otherwise = S.insert c p

producesPandigitalSeries :: Integer -> Bool
producesPandigitalSeries n
  | (length $ multiplesUntilLength n) == 0 = False
  | otherwise = isPandigitalSeries $ multiplesUntilLength n
