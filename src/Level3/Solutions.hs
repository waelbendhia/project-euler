module Level3.Solutions
  ( solutions
  ) where

import Problem

import qualified Level3.Problem51 as P51
import qualified Level3.Problem52 as P52
import qualified Level3.Problem53 as P53
import qualified Level3.Problem54 as P54

solutions :: [Problem Integer]
solutions = [P51.problem, P52.problem, P53.problem, P54.problem]
