module Solutions
  ( getProblem
  ) where

import qualified Level1.Solutions as L1
import qualified Level2.Solutions as L2
import qualified Level3.Solutions as L3
import qualified Level4.Solutions as L4
import qualified Level5.Solutions as L5
import qualified Level9.Solutions as L9
import Problem

getProblem :: Int -> Problem Integer
getProblem i =
  if length matching == 0
    then Problem i "Not solved yet" 0
    else if length matching > 1
           then error ("Found multiple solutions with number: " ++ (show i))
           else head $ matching
  where
    matching =
      filter ((== i) . ind) $
      concat
        [ L1.solutions
        , L2.solutions
        , L3.solutions
        , L4.solutions
        , L5.solutions
        , L9.solutions
        ]
