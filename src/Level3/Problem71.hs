module Level3.Problem71
  ( problem
  ) where

import Problem

problem :: Problem Integer
problem =
  Problem
  { ind = 71
  , name = "Ordered fractions"
  , solution = numerator $ biggestFracUnder 1000000 (Fraction 3 7)
  }

data Fraction =
  Fraction Integer
           Integer

instance Show Fraction where
  show (Fraction n d) = show n ++ "/" ++ show d

instance Eq Fraction where
  (Fraction an ad) == (Fraction bn bd) = (an * bd) == (bn * ad)

instance Ord Fraction where
  compare (Fraction an ad) (Fraction bn bd) = compare (an * bd) (bn * ad)

numerator :: Fraction -> Integer
numerator (Fraction n _) = n

simplify :: Fraction -> Fraction
simplify (Fraction n d) = Fraction (div n g) (div d g)
  where
    g = gcd n d

biggestFracUnder :: Integer -> Fraction -> Fraction
biggestFracUnder bound (Fraction nf df) =
  maximum $ map (\n -> simplify $ Fraction (closestUnder n) n) [1 .. bound]
  where
    closestUnder x =
      (nf * x `div` df) -
      (if rem x df == 0
         then 1
         else 0)
