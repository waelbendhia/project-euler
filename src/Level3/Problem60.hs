module Level3.Problem60
  ( problem
  ) where

import Data.Numbers.Primes

import Problem

problem :: Problem Integer
problem =
  Problem
  { ind = 60
  , name = "Prime pair sets"
  , solution =
      case dfsPrimeSet 5 1051 of
        Nothing -> 0
        Just l -> toInteger $ sum l
  }

-- We run a DFS with progressive deepening of the number of primes we are going
-- to evaluate for prime sets. This algorithm takes a while to complete, but
-- once we know the number of primes needed to produce the first 5 element
-- prime set then we just plug that into the function. It turns out that we
-- need 1051 primes to produce a 5 element long prime pair set.
getFirstPrimeSetOf :: (Integral a, Show a) => Int -> ([a], Int)
getFirstPrimeSetOf len = helper len
  where
    helper b =
      case dfsPrimeSet len b of
        Just l -> (l, b)
        Nothing -> helper (b + 1)

dfsPrimeSet :: (Eq t, Integral a, Num t, Show a) => t -> Int -> Maybe [a]
dfsPrimeSet len numPrimes = helper 0 0 primes'
  where
    helper depth val children =
      if depth == len
        then Just [val]
        else (firstJust $
              map
                (\c ->
                   helper (depth + 1) c $
                   filter (primePair c) $ dropWhile (< c) children)
                children) >>=
             (if depth /= 0
                then Just . (val :)
                else Just)
    primes' = 3 : (drop 3 $ take numPrimes primes)

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (x:xs) =
  case x of
    Nothing -> firstJust xs
    y -> y

primePair :: (Show a1, Show a) => a1 -> a -> Bool
primePair a b = isPrime (concatNum a b) && isPrime (concatNum b a)

concatNum :: (Show a1, Show a2, Read a) => a2 -> a1 -> a
concatNum a b = read $ show a ++ show b
