module Solutions
  ( getProblem
  ) where

import qualified Level1.Solutions as L1
import qualified Level13.Solutions as L13
import qualified Level14.Solutions as L14
import qualified Level15.Solutions as L15
import qualified Level16.Solutions as L16
import qualified Level17.Solutions as L17
import qualified Level18.Solutions as L18
import qualified Level2.Solutions as L2
import qualified Level20.Solutions as L20
import qualified Level21.Solutions as L21
import qualified Level22.Solutions as L22
import qualified Level3.Solutions as L3
import qualified Level4.Solutions as L4
import qualified Level5.Solutions as L5
import qualified Level6.Solutions as L6
import qualified Level8.Solutions as L8
import qualified Level9.Solutions as L9
import Problem

getProblem :: Int -> Problem Integer
getProblem i
  | null matching = Problem i "Not solved yet" 0
  | length matching > 1 =
    error ("Found multiple solutions with number: " ++ show i)
  | otherwise = head matching
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
        , L8.solutions
        , L9.solutions
        , L13.solutions
        , L14.solutions
        , L15.solutions
        , L16.solutions
        , L17.solutions
        , L18.solutions
        , L20.solutions
        , L21.solutions
        , L22.solutions
        ]
