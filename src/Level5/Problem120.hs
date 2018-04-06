module Level5.Problem120
  ( problem
  ) where

-- Expansions:
-- 1 : 2a
-- 2 : 2a^2 + 2
-- 3 : 2a^3 + 6a
-- 4 : 2a^4 + 12a^2 + 2
-- 5 : 2a^5 + 20a^3 + 10a
-- 6 : 2a^6 + 30a^4 + 30a^2 + 2
-- 7 : 2a^7 + 42a^5 + 70a^3 + 14a
-- Since we're dealing with mod a^2 we can simplify to:
-- 1 : 2a
-- 2 : 2
-- 3 : 6a
-- 4 : 2
-- 5 : 10a
-- 6 : 2
-- 7 : 14a
-- We can probably prove at this point that for every even n r is equal to 2,
-- and for evey odd n r = 2na.
-- Given this formula (2na % a^2) is maximized for any n = (k*a - 1) / 2
-- where n is integer.
-- So the formula for rMax = 2*((k*a - 1) / 2)*a % a^2
-- We can set k to 1 and remove the % a^2 part.
import Problem

problem :: Problem Integer
problem =
  Problem
  { ind = 120
  , name = "Square remainders"
  , solution = sum $ map (\a -> 2 * a * ((a - 1) `div` 2)) [3 .. 1000]
  }
