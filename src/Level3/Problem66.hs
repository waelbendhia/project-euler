module Level3.Problem66
  ( problem
  ) where

import Data.List
import Data.Maybe (fromMaybe)

import Problem

problem :: Problem Integer
problem =
  Problem
  { ind = 66
  , name = "Diophantine equation"
  , solution =
      fst $
      maximumBy (\(_, a) (_, b) -> compare a b) $
      minimalDiophantineSolutionsInRange [1 .. 1000]
  }

-- Read https://en.wikipedia.org/wiki/Pell%27s_equation
minimalDiophantineSolutionsInRange ::
     Foldable t => t Integer -> [(Integer, Integer)]
minimalDiophantineSolutionsInRange =
  concatMap
    (\d -> maybeToList $ minimalDiophantineSolution d >>= Just . ((,) d) . fst)

minimalDiophantineSolution :: Integer -> Maybe (Integer, Integer)
minimalDiophantineSolution d =
  if isSquare d
    then Nothing
    else Just $
         head $
         filter (\(h, k) -> h ^ 2 - d * (k ^ 2) == 1) $
         endlessConvergenceOfRoot d

maybeToList :: Maybe t -> [t]
maybeToList Nothing = []
maybeToList (Just x) = [x]

isSquare :: Integer -> Bool
isSquare x = (wholeRoot x) ^ 2 == x

wholeRoot :: Integer -> Integer
wholeRoot = truncate . sqrt . fromIntegral

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

endlessConvergenceOfRoot :: Integer -> [(Integer, Integer)]
endlessConvergenceOfRoot n = map toFraction $ inits $ (i : cycle l)
  where
    (i, l) = fractionalExpansion n
