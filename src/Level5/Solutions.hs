module Level5.Solutions
  ( solutions
  ) where

import qualified Level5.Problem100 as P100
import qualified Level5.Problem102 as P102
import qualified Level5.Problem104 as P104
import qualified Level5.Problem112 as P112
import qualified Level5.Problem119 as P119
import qualified Level5.Problem120 as P120
import qualified Level5.Problem123 as P123
import qualified Level5.Problem124 as P124
import qualified Level5.Problem125 as P125

import Problem

solutions :: [Problem Integer]
solutions =
  [ P100.problem
  , P102.problem
  , P104.problem
  , P112.problem
  , P119.problem
  , P120.problem
  , P123.problem
  , P124.problem
  , P125.problem
  ]
