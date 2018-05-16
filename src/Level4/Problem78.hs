module Level4.Problem78
  ( problem
  ) where

import Data.Array

import Problem

problem :: Problem Integer
problem =
  Problem 78 "Coin partitions" (toInteger $ firstPartitionDivisibleBy 1000000)

-- The previous algorithm for calculating partitions are too slow for this problem
-- so off to WikiPedia, which gives us
-- p(n) = p(n-k(1)) + p(n-k(2)) - p(n-k(3)) - p(n-k(4))...
-- where k(n) is the nth generalized pentagonal number.
-- The final algorithm is attempts to find an n in [1..10] that is divisble by
-- the argument, if none is found then that range is multiplied by 10 until a
-- result is found.
-- I used progressive deepening as I couldn't figure out a way to provide a
-- bound for the search space.
firstPartitionDivisibleBy :: Integral t => t -> Int
firstPartitionDivisibleBy n = helper 10
  where
    helper r =
      if null res
        then helper (r * 10)
        else fst $ head res
      where
        res =
          filter (divisibleBy n . fromIntegral . snd) $
          assocs $ partitionsUpTo r

divisibleBy :: Integral a => a -> a -> Bool
divisibleBy n m = rem m n == 0

generalizedPentagonalNumbers :: [Int]
generalizedPentagonalNumbers = map penta ns
  where
    penta n = (3 * n ^ 2 - n) `div` 2
    ns = concatMap (\n -> [n, -n]) [1 ..]

partitionsUpTo :: Num a => Int -> Array Int a
partitionsUpTo n = memo
  where
    memo = array (0, n) $ map (\n -> (fromIntegral n, partition n)) [0 .. n]
    partition n =
      if n <= 1
        then 1
        else sum $
             zipWith (*) (cycle [1, 1, -1, -1]) $
             map ((!) memo . (-) n) $
             takeWhile (<= n) generalizedPentagonalNumbers
