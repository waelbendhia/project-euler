module Level3.Problem64
  ( problem
  ) where

import Data.Maybe

import Problem

problem :: Problem Integer
problem =
  Problem
  { ind = 64
  , name = "Odd period square roots"
  , solution = toInteger $ length $ filter odd $ map periodLength [1 .. 10000]
  }

periodLength :: Integer -> Int
periodLength = length . snd . fractionalExpansion

fractionalExpansion :: Integer -> (Integer, [Integer])
fractionalExpansion x =
  if (wholeRoot x) ^ 2 == x
    then (x, [])
    else (head expansion, tail expansion)
  where
    expansion = map trd $ reverse $ helper [] Nothing
    helper [] _ = helper [(0, 1, wholeRoot x)] Nothing
    helper (l@((m, d, a):_)) first =
      if Just (mn, dn) == first
        then l
        else helper (newTerm : l) stopCondition
      where
        stopCondition = Just $ fromMaybe (mn, dn) first
        newTerm = (mn, dn, an)
        mn = (d * a) - m
        dn = (x - mn * mn) `div` d
        an = truncate $ fromInteger (wholeRoot x + mn) / fromInteger dn

trd :: (t1, t, t2) -> t2
trd (_, _, x) = x

wholeRoot :: Integer -> Integer
wholeRoot = truncate . sqrt . fromInteger
