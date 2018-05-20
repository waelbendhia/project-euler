module Level13.Solutions
  ( solutions
  ) where

import qualified Level13.Problem301 as P301
import qualified Level13.Problem315 as P315
import qualified Level13.Problem323 as P323

import Problem

solutions :: [Problem Integer]
solutions = [P301.problem, P315.problem, P323.problem]
