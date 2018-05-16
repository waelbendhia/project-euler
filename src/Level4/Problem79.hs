module Level4.Problem79
  ( problem
  ) where

import Data.List

import Problem

problem :: Problem Integer
problem = Problem 79 "Passcode derivation" (read $ crack logins)

-- Build a graph from all passcodes and then progressively remove vertices
-- with indegree == 0
crack :: [String] -> String
crack attempts = helper vertices edges
  where
    helper [] _ = []
    helper vs es = out : helper remVs remEs
      where
        out = head $ filter (isOnlyOutVertex es) vs
        remVs = filter (/= out) vs
        remEs = removeVertex out es
    edges = edgesFromAttempts attempts
    vertices = digitsFromAttempts attempts

isOnlyOutVertex :: (Eq a1, Foldable t) => t (a, a1) -> a1 -> Bool
isOnlyOutVertex g v = all ((/= v) . snd) g

digitsFromAttempts :: [String] -> String
digitsFromAttempts = map head . group . sort . concat

removeVertex :: Char -> [(Char, Char)] -> [(Char, Char)]
removeVertex v = filter ((/= v) . snd) . filter ((/= v) . fst)

edgesFromAttempts :: [String] -> [(Char, Char)]
edgesFromAttempts [] = []
edgesFromAttempts ([a, b, c]:keys) = [(a, b), (b, c)] ++ edgesFromAttempts keys

logins :: [String]
logins =
  [ "319"
  , "680"
  , "180"
  , "690"
  , "129"
  , "620"
  , "762"
  , "689"
  , "762"
  , "318"
  , "368"
  , "710"
  , "720"
  , "710"
  , "629"
  , "168"
  , "160"
  , "689"
  , "716"
  , "731"
  , "736"
  , "729"
  , "316"
  , "729"
  , "729"
  , "710"
  , "769"
  , "290"
  , "719"
  , "680"
  , "318"
  , "389"
  , "162"
  , "289"
  , "162"
  , "718"
  , "729"
  , "319"
  , "790"
  , "680"
  , "890"
  , "362"
  , "319"
  , "760"
  , "316"
  , "729"
  , "380"
  , "319"
  , "728"
  , "716"
  ]
