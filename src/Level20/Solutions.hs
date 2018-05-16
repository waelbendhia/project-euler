module Level20.Solutions
  ( solutions
  ) where

import qualified Level20.Problem493 as P493
import qualified Level20.Problem500 as P500
import qualified Level20.Problem504 as P504

import Problem

solutions :: [Problem Integer]
solutions = [P493.problem, P500.problem, P504.problem]
