module Level4.Problem99
  ( problem
  ) where

import Data.List
import Level4.Problem99Numbers
import Problem

problem :: Problem Integer
problem =
  Problem
  { ind = 99
  , name = "Largest exponential"
  , solution =
      fst $
      maximumBy
        (\a b ->
           compareExp
             (mapTuple fromInteger $ snd a)
             (mapTuple fromInteger $ snd b)) $
      zip [1 ..] baseExps
  }

-- We know that log(x^a) = a*log(x) and that if x > y then log(x)>log(y)
-- so to compare two base and exponent pairs we can compare their logarithms
compareExp :: (Floating a, Ord a) => (a, a) -> (a, a) -> Ordering
compareExp (b1, e1) (b2, e2) = compare (e1 * log b1) (e2 * log b2)

mapTuple :: (t1 -> t) -> (t1, t1) -> (t, t)
mapTuple f (a, b) = (f a, f b)
