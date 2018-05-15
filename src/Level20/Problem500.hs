module Level20.Problem500
  ( problem
  ) where

import Data.IntMap
import Data.List
import Data.Numbers.Primes
import Problem

-- Alright then let's think this through.
-- A thing I know because I read it on the internet is that for a number n
-- of prime factorization p1^k1 * p2^k2 * ... * pi^ki its number of factors is
-- (k1+1)*(k2+1)*...*(ki+1)
-- We could demonstrate this probably by doing a bunch of math but no.
-- So we could generate the smallest n divisable number by exponentiating on
-- the first x primes in a way that the above formula yields n.
-- For the base case where all primes are of exponent 1 we need log2(n) primes
-- to cover our bases. So for 2^500500 we need at most 500500 primes.
-- Ok next step is let's say we've solved for 2^(n-1) we know that for 2^n the
-- solution is going to be a multiple of the solution 2^(n-1) either by the next
-- prime which would double the number of factors or by raising the exponent of
-- one of the existing primes in a way that doubles the number of factors.
-- So if an exponent is k we need to raise it by k + 1 as (k+k+1) = 2 (k+1)
-- It's evident that we can't raise the exponent of multiple primes as we'd need
-- to raise them in a way that it would double the number of factors but 2 has
-- no factors other than 1 and 2 (This is a lot clearer in my head but trust me
-- I'm sure of this).
-- So for each step the factor is going to be the minimum of the next prime or
-- one of the existing primes raised to k+1 exponent.
problem :: Problem Integer
problem = Problem 500 "Problem 500!" (solve 500500)

solveNumberOfFactors2Exp n = f 0 1 [] primes
  where
    f numFactors val factors (p:ps)
      | numFactors == n = val
      | otherwise =
        f
          (numFactors + 1)
          ((val * (fst candidate ^ snd candidate)) `mod` 500500507)
          (insertF candidate factors)
          (if p == fst candidate
             then ps
             else p : ps)
      where
        candidate =
          minimumBy (\a b -> compare (fst a ^ snd a) (fst b ^ snd b)) $
          (p, 1) : fmap (\(p', exp) -> (p', exp + 1)) factors

insertF :: (Num b, Eq a) => (a, b) -> [(a, b)] -> [(a, b)]
insertF x [] = [x]
insertF (p, exp) ((p', exp'):ps)
  | p == p' = (p, exp + exp') : ps
  | otherwise = (p', exp') : insertF (p, exp) ps

insertSorted x [] = [x]
insertSorted x (p:ps)
  | uncurry (^) p < uncurry (^) x = p : insertSorted x ps
  | otherwise = x : p : ps

solve target = f 0 1 $ take target $ zip primes $ repeat 1
  where
    f n cur ((next, exp):queue)
      | n == target = cur
      | otherwise = f (n + 1) nextVal nextQueue
      where
        nextVal = (cur * next ^ exp) `mod` 500500507
        nextQueue = insertSorted (next, 2 * exp) queue
