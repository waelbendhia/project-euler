module Level3.Problem68
  ( problem
  ) where

import Data.List
import Data.Maybe

import Problem

problem :: Problem Integer
problem =
  Problem
    68
    "Magic 5-gon ring"
    (read $
     maximum $ filter ((== 16) . length) $ map fiveGonToString validFiveGons)

data Line =
  Line Integer
       Integer
  deriving (Show)

data FiveGon =
  FiveGon Line
          Line
          Line
          Line
          Line
  deriving (Show)

validate :: FiveGon -> Bool
validate (FiveGon (Line a b) (Line c d) (Line e f) (Line g h) (Line i j)) =
  a < c &&
  a < e &&
  a < g &&
  a < i && line1 == line2 && line2 == line3 && line3 == line4 && line4 == line5
  where
    line1 = a + j + b
    line2 = c + b + d
    line3 = e + d + f
    line4 = g + f + h
    line5 = i + h + j

fiveGonFromList :: [Integer] -> Maybe FiveGon
fiveGonFromList [a, b, c, d, e, f, g, h, i, j] =
  if validate gon
    then Just $ gon
    else Nothing
  where
    gon = FiveGon (Line a b) (Line c d) (Line e f) (Line g h) (Line i j)
fiveGonFromList _ = Nothing

validFiveGons :: [FiveGon]
validFiveGons =
  concatMap maybeToList $ map fiveGonFromList $ permutations [1 .. 10]

fiveGonToString :: FiveGon -> [Char]
fiveGonToString (FiveGon (Line a b) (Line c d) (Line e f) (Line g h) (Line i j)) =
  concatMap show [a, j, b, c, b, d, e, d, f, g, f, h, i, h, j]
