module Level1.Problem22
  ( problem
  ) where

import Data.Char
import Data.List
import Level1.Problem22Names
import Problem

problem :: Problem Integer
problem = Problem {ind = 22, name = "Names scores", solution = sumScoreAllNames}

sumScoreAllNames :: Integer
sumScoreAllNames = sum $ map (toInteger . scoreName) names

scoreName :: [Char] -> Int
scoreName n = scoreLetters n * position n

scoreLetters :: [Char] -> Int
scoreLetters n = sum $ map (\c -> (ord $ toLower c) - ord 'a' + 1) n

position :: [Char] -> Int
position n =
  case elemIndex n names of
    Just i -> i + 1
    Nothing -> 0
