module Level17.Problem407
  ( problem
  ) where

import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.Foldable
import Data.Numbers.Primes

import Problem

problem :: Problem Integer
problem = Problem 407 "Idempotents" (toInteger $ sumCapitalMs $ 10 ^ 7)

-- So if a^2 === a mod n then a(a-1) == k*n where k is an integer
-- This gives us a few insights:
-- First if n is a prime then M(n) is 1 since there exists no a for which
-- n == a(a-1)/k other than 1.
-- Second if n is a prime power then M(n) is also 1 as there exists no a for
-- which p^m = a(a-1)/k
-- Finally M(n) or M(n)+1 is a factor of n.
-- So to calculate M(n) we simply need to iterate through multiples of the
-- largest prime factor of n.
-- To speed things up we create a lookup function for largest prime factors of
-- a number
-- The algorithm takes 109 seconds to terminate which is kind of on the high
-- side but I'm okay with it.
-- There might be some optimizations I'm missing, but more likely than not my
-- approach is not optimal.
capitalM :: Integral t => (t -> t) -> t -> t
capitalM fLPF n
  | null res = 1
  | otherwise = head res
  where
    res = do
      a <- map (* fact) [n `div` fact,(n `div` fact) - 1 .. 1]
      if (a + 1) ^ 2 `mod` n == a + 1
        then return $ a + 1
        else if (a ^ 2) `mod` n == a
               then return a
               else []
    fact = fLPF n

sumCapitalMs :: Int -> Int
sumCapitalMs b = sum $ capitalM fLPF <$> [2 .. b]
  where
    fLPF = createLargestPrimeLookup b

createLargestPrimeLookup :: Int -> Int -> Int
createLargestPrimeLookup limit = (lookUpArr !)
  where
    lookUpArr =
      runSTArray $ do
        arr <- newArray (1, limit) 1 :: ST s (STArray s Int Int)
        forM_ [2 .. limit] $ \i -> do
          p <- readArray arr i
          if p > 1
            then return ()
            else let multiples = takeWhile (< limit) $ (* i) <$> [1 ..]
                  in forM_ multiples $ flip (writeArray arr) i
        return arr
