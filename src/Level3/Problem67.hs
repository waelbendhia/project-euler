module Level3.Problem67
  ( problem
  ) where

import Level3.Problem67Triangle
import Problem

problem :: Problem Integer
problem =
  Problem
  { ind = 67
  , name = "Maximum path sum II"
  , solution = head $ maxPath theTriangle
  }

maxPath :: (Ord a, Num a) => [[a]] -> [a]
maxPath [] = [0]
maxPath [leaves] = maxAdjacent leaves
maxPath (l1:l2) = maxAdjacent $ zipWith (+) l1 $ maxPath l2

maxAdjacent :: Ord a => [a] -> [a]
maxAdjacent [] = []
maxAdjacent [x] = [x]
maxAdjacent (x1:x2:xs) = max x1 x2 : maxAdjacent (x2 : xs)
