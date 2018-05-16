module Level3.Problem65
  ( problem
  ) where

import Data.Char
import Problem

problem :: Problem Integer
problem =
  Problem
    65
    "Convergents of e"
    (toInteger $
     sum $
     map digitToInt $ show $ fst $ fractionFromConvergent $ nthConvergentOfe 100)

nthConvergentOfe :: (Enum a, Num a, Num t) => Int -> (t, [a])
nthConvergentOfe n = (2, take (n - 1) expansionOfe)
  where
    expansionOfe =
      zipWith ($) (cycle [const 1, (* 2), const 1]) $
      concatMap (replicate 3) [1 ..]

fractionFromConvergent :: Integral t => (t, [t]) -> (t, t)
fractionFromConvergent (f, l) = toFraction (f : l)

toFraction :: Integral t => [t] -> (t, t)
toFraction [] = (0, 0)
toFraction [t] = (t, 1)
toFraction (t:rest) = addToFraction t (d, n)
  where
    (n, d) = toFraction rest

simplify :: Integral t => (t, t) -> (t, t)
simplify (n, d) = (div n g, div d g)
  where
    g = gcd n d

addToFraction :: Integral t => t -> (t, t) -> (t, t)
addToFraction t (n, d) = simplify (t * d + n, d)
