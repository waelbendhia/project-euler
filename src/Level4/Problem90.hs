module Level4.Problem90
  ( problem
  ) where

import Data.List

import Problem

-- I feel there's probably a faster solution using graphs, but since the
-- search space is small enough a brute force solution is adequate.
-- Also I don't see why this problem has a difficulty rating of 40%.
problem :: Problem Integer
problem =
  Problem
    90
    "Cube digit pairs"
    (toInteger $
     length $
     filter (uncurry $ produces (allCubesUnder 100)) (allDicePairs $ allDice 6))

produces :: (Num a, Eq a, Foldable t) => t a -> [a] -> [a] -> Bool
produces ns d1 d2 =
  (length ns ==) $
  length $ nub $ filter (\x -> any (== x) ns) $ allNumbersProducedBy d1 d2

allDice :: (Enum a, Num a) => Int -> [[a]]
allDice sides = filter ((== sides) . length) $ subsequences [0 .. 9]

allCubesUnder :: (Enum a, Ord a, Num a) => a -> [a]
allCubesUnder n = takeWhile (< n) $ map (^ 2) $ [1 ..]

allNumbersProducedBy :: (Eq a, Num a) => [a] -> [a] -> [a]
allNumbersProducedBy d1 d2 =
  [10 * s1 + s2 | s1 <- d1', s2 <- d2'] ++ [10 * s2 + s1 | s1 <- d1', s2 <- d2']
  where
    extend d =
      if any (== 6) d
        then 9 : d
        else if any (== 9) d
               then 6 : d
               else d
    d1' = extend d1
    d2' = extend d2

allDicePairs :: [b] -> [(b, b)]
allDicePairs [] = []
allDicePairs (d:ds) = map ((,) d) ds ++ allDicePairs ds
