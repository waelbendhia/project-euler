module Level4.Problem88
  ( problem
  ) where

import Data.Array
import Data.List

import Problem

problem :: Problem Integer
problem =
  Problem
  {ind = 88, name = "Product-sum numbers", solution = sumMinProdSumUnder 12000}

-- So this took some thinking. The first part of the problem is how to provide
-- bounds to the search space. A lower bound is evident, the lowest number is k
-- since a sum of k 1s is k. For a product sum number of k size set the highest
-- factor we could have is k however in that case the next biggest factor can
-- only be 2 as in any other case the sum will always be inferior to the
-- product of the set. So we end up with
--  k =< minProdSum(k) <= k*2.
-- This was a problem that caused me quite a bit of trouble since my first
-- approach was to find a way to determine the MPS for a given k then sum them
-- all up. Until I realized that trying find all factorizations for a certain
-- number was silly.
-- The final solution was to simply examine all possible factorizations that
-- yield a number below the given threshold and keep a cache of the smalled
-- MPS.
-- The final solution is somewhat dissapointingly simple.
sumMinProdSumUnder :: Integer -> Integer
sumMinProdSumUnder = sum . nub . elems . allMinProdSum

allMinProdSum :: (Integral e, Ix e) => e -> Array e e
allMinProdSum kMax
  -- Begining with {1} we start testing all possible factorizations
  -- recursively.
  -- Given the top bound mps < 2*k the branching factor is not that high and
  -- actually decreases the closer we get to our top threshold.
  -- And if we're careful to not retest the same factorization we end up
  -- at solution fairly quickly.
  -- While the final solution runs in a low exponential time.
 = helper 1 1 1 2 $ listArray (2, kMax) $ map (2 *) [2 .. kMax]
  where
    helper p s l str arr =
      if k <= kMax
        then foldl
               (\prev i -> helper (p * i) (s + i) (l + 1) i prev)
               arr'
               -- we know that minProd sum is <= to 2 * k so we only need to
               -- examine up to (2k+1/p)
               [str .. 2 * quot (1 + kMax) p]
        else arr'
      where
        k = p - s + l
        arr' =
          if k >= 2 && k <= kMax && p < arr ! k
            then arr // [(k, p)]
            else arr
