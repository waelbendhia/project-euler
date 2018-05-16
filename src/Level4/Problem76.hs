module Level4.Problem76
  ( problem
  ) where

import qualified Data.IntMap as M

import Problem

problem :: Problem Integer
problem = Problem 76 "Counting summations" (summationsFor 100)

-- This problem resembles problem 31 except it's integers instead of coins
summationsFor :: Num a => M.Key -> a
summationsFor val = (helper [1 .. val] 0 (M.singleton 0 1) M.! val) - 1
  where
    (!?) m k =
      if M.member k m
        then m M.! k
        else 0
    helper [] _ memo = memo
    helper (n:ns) v memo =
      if v > val
        then helper ns 0 memo
        else helper (n : ns) (v + 1) $ M.insertWith (+) v (memo !? (v - n)) memo
