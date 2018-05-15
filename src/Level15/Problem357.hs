module Level15.Problem357
  ( problem
  ) where

import Control.Monad.ST
import Data.Array
import Data.Array.MArray
import Data.Array.ST
import Data.Foldable
import Data.Numbers.Primes (primes)

import Problem

-- Algorithm is trivial, runs in 43 seconds but it's my first time using the ST
-- Monad! So yay.
problem :: Problem Integer
problem = Problem 357 "Prime generating integers" (sum $ pgiUnder $ 10 ^ 8)

createPrimeLookup :: Integer -> ST s (STArray s Integer Bool)
createPrimeLookup b = do
  arr <- newArray (1, b + 4) False :: ST s (STArray s Integer Bool)
  forM_ (takeWhile (< b) primes) $ flip (writeArray arr) True
  return arr

pgiUnder :: Integer -> [Integer]
pgiUnder b = (:) 1 $ filter checker $ map (+ 2) [0,4 .. b]
  where
    luTable = runSTArray $ createPrimeLookup b
    isPrime x = luTable ! x
    checker n = helper 1
      where
        helper d
          | d * d > n = True
          | n `rem` d == 0 =
            if isPrime (d + n `quot` d)
              then helper (d + 1)
              else False
          | otherwise = helper (d + 1)
