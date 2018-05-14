module Level16.Solutions
  ( solutions
  ) where

import qualified Level16.Problem381 as P381
import qualified Level16.Problem387 as P387

import Problem

solutions :: [Problem Integer]
solutions = [P381.problem, P387.problem]
