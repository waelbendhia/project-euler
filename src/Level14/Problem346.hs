module Level14.Problem346
  ( problem
  ) where

import Data.Set (Set, fromList)

import Problem

problem :: Problem Integer
problem =
  Problem
  { ind = 346
  , name = "Strong Repunits"
  , solution = (+ 1) $ sum $ strongRepUnits $ 10 ^ 12
  }

generateRepUnits :: Num b => b -> [b]
generateRepUnits b = map (+ 1) $ scanl (+) 0 $ map (b ^) [1 ..]

strongRepUnits :: Integer -> Set Integer
strongRepUnits lim =
  fromList $
  [2 .. intSqrt lim] >>= takeWhile (< lim) . drop 2 . generateRepUnits

intSqrt :: Integer -> Integer
intSqrt = truncate . sqrt . fromIntegral
