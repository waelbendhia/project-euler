module Level2.Problem32
  ( problem
  ) where

import Data.Char
import qualified Data.Set as S

import Problem

problem :: Problem Integer
problem = Problem 32 "Pandigital products" (sum unusuals)

-- We do the extra toList.fromList step to eliminate duplicate products such
-- as 12*483 and 42*138
unusuals :: [Integer]
unusuals =
  S.toList $ S.fromList $ map (uncurry (*)) $ filter checker possibleMultipliers
  where
    checker (m1, m2) =
      isPandigital $ read $ show m1 ++ show m2 ++ show (m1 * m2)

containsRepeatingDigits :: Show a => a -> Bool
containsRepeatingDigits =
  (== 0) . S.size . foldl folder (S.singleton 0) . map digitToInt . show
  where
    folder p c
      | S.size p == 0 = S.empty
      | S.member c p = S.empty
      | otherwise = S.insert c p

isPandigital :: Integer -> Bool
isPandigital n = (9 == length (show n)) && (not . containsRepeatingDigits) n

-- We know that the sum of digits has to be 9. We can check for each numbers
-- of digits in the factors what number of digits the product is. After 
-- calculations we find that the multiplier lengths can be either 2 and 3 or
-- 1 and 4. So a possible multiplier will be in the range 1-9876, have no
-- repeating digits and not contain any zeroes.
possibleMultiplier :: [Integer]
possibleMultiplier = filter (not . containsRepeatingDigits) [1 .. 9876]

possibleMultipliers :: [(Integer, Integer)]
possibleMultipliers =
  [(m1, m2) | m1 <- multsOfLength 1, m2 <- multsOfLength 4] ++
  [(m1, m2) | m1 <- multsOfLength 2, m2 <- multsOfLength 3]
  where
    multsOfLength n = filter ((== n) . length . show) possibleMultiplier
