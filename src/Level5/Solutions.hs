module Level5.Solutions
  ( solutions
  ) where

import qualified Level5.Problem100 as P100
import qualified Level5.Problem102 as P102
import qualified Level5.Problem104 as P104
import qualified Level5.Problem112 as P112
import qualified Level5.Problem124 as P124

import Problem

solutions :: [Problem Integer]
solutions =
  [P100.problem, P102.problem, P104.problem, P112.problem, P124.problem]
