module Level4.Problem95
  ( problem
  ) where

import qualified Data.IntMap.Lazy as M
import Data.List
import Problem

-- This solution can be significanly sped up by checking if any number
-- encountered in a chain already forms an amicable chain
-- however this type of algorithm does not lend itself well to Haskell
-- or more likely I am not yet adept enough to compose it properly.
-- However the brute force algorithm yields a solution in under a minute
-- on my two year old HP Envy 13.
problem :: Problem Integer
problem =
  Problem
  { ind = 95
  , name = "Amicable chains"
  , solution =
      toInteger $
      minimum $
      maximumBy (\a b -> compare (length a) (length b)) $
      filter isAmicableChain $ allChainsBounded $ 10 ^ 6
  }

sumDivisors :: Int -> Int
sumDivisors = (+ 1) . sum . divisors

divisors :: Integral a => a -> [a]
divisors n = filter (/= n) $ filtered ++ map (quot n) filtered
  where
    filtered = filter ((==) 0 . rem n) [2 .. intSqrt n]
    intSqrt = truncate . sqrt . fromIntegral

makeChainFromBounded :: Int -> Int -> [Int]
makeChainFromBounded bound x =
  unfoldr
    (\(y, dict) ->
       if y > bound || M.member y dict
         then Nothing
         else Just (y, (sumDivisors y, M.insert y True dict)))
    (x, M.empty)

isAmicableChain :: [M.Key] -> Bool
isAmicableChain l = length l > 0 && head l == (sumDivisors $ last l)

allChainsBounded :: Int -> [[Int]]
allChainsBounded bound = map (makeChainFromBounded $ bound) [0 .. bound]
