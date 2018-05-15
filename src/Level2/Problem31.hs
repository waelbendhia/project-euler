module Level2.Problem31
  ( problem
  ) where

import qualified Data.IntMap.Lazy as M
import Problem

problem :: Problem Integer
problem =
  Problem 31 "Coin sums" (coinCombinations [1, 2, 5, 10, 20, 50, 100, 200] 200)

coinCombinations :: [M.Key] -> M.Key -> Integer
coinCombinations coins val = (coin coins 0 (M.singleton 0 1)) M.! val
  where
    safeLookUp m k
      | M.member k m = m M.! k
      | otherwise = 0
    coin [] _ memo = memo
    coin (c:cs) v memo
      | v > val = coin (cs) 0 memo
      | otherwise =
        coin
          (c : cs)
          (v + 1)
          (M.insertWith (+) v (safeLookUp memo (v - c)) memo)
