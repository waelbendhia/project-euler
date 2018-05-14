module Level1.Problem23
  ( problem
  ) where

import qualified Data.Set as S
import Problem

problem :: Problem Integer
problem =
  Problem
    { ind = 23
    , name = "Non-abundant sums"
    , solution = sumOfAllNonAbundantSumNumbers
    }

sumOfAllNonAbundantSumNumbers :: Integer
sumOfAllNonAbundantSumNumbers =
  sum $ filter (not . isSumOfAbundantNumbers) [1 .. 21823]

isSumOfAbundantNumbers :: Integer -> Bool
isSumOfAbundantNumbers x = check 1
  where
    check n
      | n > (quot x 2) = False
      | S.member n abundantNumbers && S.member (x - n) abundantNumbers = True
      | otherwise = check (n + 1)

abundantNumbers :: S.Set Integer
abundantNumbers = S.fromList $ filter isAbundant [1 .. 21823]

divisors :: Integer -> [Integer]
divisors n =
  filter (/= n) $
  1 :
  filtered ++
  (map (quot n) $ filter ((/=) (sqrt $ fromIntegral n) . fromIntegral) filtered)
  where
    filtered = filter ((==) 0 . rem n) [2 .. truncate $ sqrt $ fromIntegral n]

isAbundant :: Integer -> Bool
isAbundant n = (sum $ divisors n) > n
