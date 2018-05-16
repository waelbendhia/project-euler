module Level4.Problem93
  ( problem
  ) where

import Data.Char
import Data.List
import Problem

problem :: Problem Integer
problem =
  Problem
    93
    "Arithmetic expressions"
    (read $
     map (intToDigit . truncate) $
     fst $
     maximumBy (\(_, a) (_, b) -> compare a b) $
     map
       (\s ->
          ( s
          , length $
            getConsecutive $ dropWhile (<= 0) $ filter isInt $ allResults s))
       all4DigitSets)

all4DigitSets :: [[Double]]
all4DigitSets = do
  a <- [1 .. 9]
  b <- [a + 1 .. 9]
  c <- [b + 1 .. 9]
  d <- [c + 1 .. 9]
  return [a, b, c, d]

divs :: (Fractional a, Eq a) => a -> a -> [a]
divs _ 0 = [0]
divs 0 _ = [0]
divs a b = [a / b, b / a]

subs :: (Num a, Eq a) => a -> a -> [a]
subs a b =
  if a == b
    then [0]
    else [a - b, b - a]

allResults :: (Fractional a, Ord a) => [a] -> [a]
allResults [] = []
allResults [a, b] = nub $ sort $ a * b : a + b : divs a b ++ subs a b
allResults l =
  nub $
  sort
    (do a <- l
        b <- allResults $ delete a l
        allResults [a, b])

getConsecutive :: (Num a, Eq a) => [a] -> [a]
getConsecutive [] = []
getConsecutive [x] = [x]
getConsecutive (x1:x2:xs) =
  if x1 + 1 == x2
    then x1 : getConsecutive (x2 : xs)
    else [x1]

isInt :: RealFrac a => a -> Bool
isInt x = x == fromInteger (round x)
