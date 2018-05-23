module Level20.Solutions
  ( solutions
  ) where

import qualified Level20.Problem491 as P491
import qualified Level20.Problem493 as P493
import qualified Level20.Problem500 as P500

import Problem

solutions :: [Problem Integer]
solutions = [P491.problem, P493.problem, P500.problem]
