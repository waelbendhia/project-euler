module Level22.Problem549
  ( problem
  ) where

import Control.Monad.ST
import Data.Array
import Data.Array.MArray
import Data.Array.ST
import Data.Foldable
import Data.Numbers.Primes (primes)

import Problem

-- Okay, first how the fuck is this a 10% problem? 
-- So it turns out that this is called the Kempner function or the Smarandache
-- function we'll call it k. It turns out that k(p^e*a) = max(k(p^e), k(a))
-- where p is a prime. Going off that I've build a dynamic programming 
-- algorithm to solve all k under a given bound.
-- For k(p^e) the solution is either p*e for e <= p or some other thing.
-- You can find a better explanation by Googling Kempner function.
-- The program takes 199.32s to solve and I'm probably missing some easy
-- optimizations but I'm kind of over this problem.
problem :: Problem Integer
problem =
  Problem
  { ind = 549
  , name = "Divisibility of factorials"
  , solution = (subtract 1) $ sum $ kempners $ 10 ^ 6
  }

createPrimeLookup :: Integer -> ST s (STArray s Integer Bool)
createPrimeLookup b = do
  arr <- newArray (1, b + 4) False :: ST s (STArray s Integer Bool)
  forM_ (takeWhile (< b) primes) $ flip (writeArray arr) True
  return arr

kempners :: Integer -> Array Integer Integer
kempners b = r
  where
    isPrime' = (lookup !)
    lookup = runSTArray $ createPrimeLookup b
    r = listArray (1, b) $ map k [1 .. b]
    k n
      | n == 1 = 1
      | otherwise =
        if p ^ e == n
          then kempPrimePower p (fromIntegral e)
          else max (r ! (p ^ e)) (r ! (n `div` p ^ e))
      where
        (p, e) = smallestPrimePower isPrime' n

smallestPrimeFactor :: Integral a => (a -> Bool) -> a -> a
smallestPrimeFactor isP n
  | even n = 2
  | isP n = n
  | otherwise = head $ filter ((== 0) . (n `rem`)) $ tail primes

smallestPrimePower :: (Integral a, Num b) => (a -> Bool) -> a -> (a, b)
smallestPrimePower isP n = (spf, f (n `div` spf) 1)
  where
    spf = smallestPrimeFactor isP n
    f r e
      | r `rem` spf /= 0 = e
      | otherwise = f (r `div` spf) (e + 1)

kempPrimePower :: Integer -> Integer -> Integer
kempPrimePower p e
  | e <= p = p * e
  | otherwise = (p - 1) * e + (sum $ ks (e `quot` a v) (v - 1) (e `rem` a v))
  where
    v = floor $ logBase (fromIntegral p) $ fromIntegral $ 1 + e * (p - 1)
    a n = (p ^ n - 1) `quot` (p - 1)
    ks k an r
      | r == 0 = [k]
      | otherwise = k : ks (r `quot` a an) (an - 1) (r `rem` a an)
    imLog :: Integer -> Integer -> Integer
    imLog b x =
      if x < b
        then 0
        else let l = 2 * imLog (b * b) x
                 doDiv x l =
                   if x < b
                     then l
                     else doDiv (x `div` b) (l + 1)
             in doDiv (x `div` (b ^ l)) l
