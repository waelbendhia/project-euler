module Level1.Problem14
  ( problem
  ) where

import qualified Data.IntMap.Lazy as M
import Problem

problem :: Problem Integer
problem =
  Problem
    { ind = 14
    , name = "Longest Collatz sequence"
    , solution = fromIntegral $ solver 1000000
    }

solver :: Int -> Int
solver bound = fst $ highestUnder bound

highestUnder :: Int -> (Int, Integer)
highestUnder n =
  M.foldlWithKey
    (\(p, pm) k a ->
       if a > pm && k < n
         then (k, a)
         else (p, pm))
    (0, 0) $
  allCollatzLengthsUnder n

allCollatzLengthsUnder :: Int -> M.IntMap Integer
allCollatzLengthsUnder n =
  M.filterWithKey (\k _ -> k < n) $ buildUpDictionary n (M.singleton 1 1)
  where
    buildUpDictionary n dict
      | n == 1 = dict
      | otherwise = buildUpDictionary (n - 1) d
      where
        d = collatzLengths n dict

collatzLengths :: Int -> M.IntMap Integer -> M.IntMap Integer
collatzLengths n dict
  | M.member n dict = dict
  | otherwise = M.insert n (1 + nextLen) nextDict
  where
    nextDict = collatzLengths nextVal dict
    nextVal =
      if even n
        then quot n 2
        else n * 3 + 1
    nextLen = (collatzLengths nextVal dict) M.! nextVal
