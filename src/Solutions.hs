module Solutions
  ( getProblem
  ) where

import qualified Level1.Solutions as L1
import qualified Level13.Solutions as L13
import qualified Level14.Solutions as L14
import qualified Level15.Solutions as L15
import qualified Level16.Solutions as L16
import qualified Level2.Solutions as L2
import qualified Level20.Solutions as L20
import qualified Level22.Solutions as L22
import qualified Level3.Solutions as L3
import qualified Level4.Solutions as L4
import qualified Level5.Solutions as L5
import qualified Level6.Solutions as L6
import qualified Level7.Solutions as L7
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
        , L6.solutions
        , L7.solutions
        , L9.solutions
        , L13.solutions
        , L14.solutions
        , L15.solutions
        , L16.solutions
        , L20.solutions
        , L22.solutions
        ]
