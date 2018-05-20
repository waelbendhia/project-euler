module Level13.Problem323
  ( problem
  ) where

import Problem

-- For each y the probabiblity of any bit in x being unset is decreases by 50%.
-- So for any bit in xi its probability of being unset is 0.5^i and its
-- probability of being set is 1-0.5^i.
-- Finally the probability of all bits  being set is (1-0.5^i)^32 and the
-- probability of one bit being zero is 1 - (1-0.5^i)^32.
-- So to get our result we can sum all probabilities of one bit being zero for
-- each iteration until we reach the desired precision, or just do it one
-- thousand times.
problem :: Problem Integer
problem =
  Problem
    323
    "Bitwise-OR operations on random integers"
    (truncate $ 10 ^ 10 * expectedValueForFirstIs 1000)

expectedValueForFirstIs :: (Integral b, Fractional a) => b -> a
expectedValueForFirstIs i = sum $ (1 -) . probabiblityOfAll1 <$> [0 .. i]

probabiblityOfAll1 :: (Fractional a, Integral b) => b -> a
probabiblityOfAll1 i = (1 - 0.5 ^ i) ^ 32
