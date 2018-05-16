module Level3.Problem53
  ( problem
  ) where

import Problem

problem :: Problem Integer
problem =
  Problem
    53
    "Combinatoric selections"
    (toInteger $
     length $ filter ((>= 1000000) . uncurry comb) $allPairsUnder 100)

allPairsUnder :: (Enum b, Num b) => b -> [(b, b)]
allPairsUnder x = concatMap (\n -> map ((,) n) [1 .. n]) [1 .. x]

comb :: Integral t => t -> t -> t
comb n r = fact n `div` (fact r * fact (n - r))

fact :: (Num t, Ord t) => t -> t
fact n =
  if n <= 1
    then 1
    else n * fact (n - 1)
