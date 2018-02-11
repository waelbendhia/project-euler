module Level4.Solutions
  ( solutions
  ) where

import qualified Level4.Problem76 as P76
import qualified Level4.Problem77 as P77
import qualified Level4.Problem79 as P79
import qualified Level4.Problem92 as P92
import qualified Level4.Problem97 as P97

import Problem

solutions :: [Problem Integer]
solutions = [P76.problem, P77.problem, P79.problem, P92.problem, P97.problem]
