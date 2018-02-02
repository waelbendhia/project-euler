module Solutions
  ( getProblem
  ) where

import qualified Level1.Solutions as L1
import qualified Level2.Solutions as L2
import Problem

getProblem :: Int -> Problem Integer
getProblem i =
  if length matching == 0
    then Problem i "Not solved yet" 0
    else head $ matching
  where
    matching = filter ((== i) . ind) $ concat [L1.solutions, L2.solutions]
