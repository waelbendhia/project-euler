module Level1.Problem22
  ( problem
  ) where

import Data.Char
import Data.List
import Level1.Problem22Names
import Problem

problem :: Problem Integer
problem = Problem 22 "Names scores" sumScoreAllNames

sumScoreAllNames :: Integer
sumScoreAllNames = sum $ map (toInteger . scoreName) names

scoreName :: String -> Int
scoreName n = scoreLetters n * position n

scoreLetters :: String -> Int
scoreLetters n = sum $ map (\c -> ord (toLower c) - ord 'a' + 1) n

position :: String -> Int
position n =
  case elemIndex n names of
    Just i -> i + 1
    Nothing -> 0
