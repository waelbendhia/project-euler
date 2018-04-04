module Level5.Problem102
  ( problem
  ) where

import Level5.Problem102Triangles
import Problem

problem :: Problem Integer
problem =
  Problem
  { ind = 102
  , name = "Triangle containment"
  , solution =
      toInteger $ length $ filter (flip isInTriangle $ (0, 0)) triangles
  }

isInTriangle ::
     (Integral t, Ord t) => ((t, t), (t, t), (t, t)) -> (t, t) -> Bool
isInTriangle ((x1, y1), (x2, y2), (x3, y3)) (x, y) =
  all (> 0) [alpha, beta, gamma]
  where
    alpha =
      (toRational ((y2 - y3) * (x - x3) + (x3 - x2) * (y - y3))) /
      (toRational ((y2 - y3) * (x1 - x3) + (x3 - x2) * (y1 - y3)))
    beta =
      (toRational ((y3 - y1) * (x - x3) + (x1 - x3) * (y - y3))) /
      (toRational ((y2 - y3) * (x1 - x3) + (x3 - x2) * (y1 - y3)))
    gamma = 1 - alpha - beta

triangleA :: ((Integer, Integer), (Integer, Integer), (Integer, Integer))
triangleA = ((-340, 495), (-153, -910), (835, -947))

triangleB :: ((Integer, Integer), (Integer, Integer), (Integer, Integer))
triangleB = ((-175, 41), (-421, -714), (574, -645))

point = (0, 0)
