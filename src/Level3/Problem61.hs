module Level3.Problem61
  ( problem
  ) where

import Data.Maybe

import Problem

problem :: Problem Integer
problem =
  Problem
  { ind = 61
  , name = "Cyclical figurate numbers"
  , solution = head $ map sum solve
  }

-- This solution is a bit verbose but it's late and I'm tired. Basically it's
-- just a DFS where for each node a child as any number that belongs to a set
-- from a set not yet expanded into which is cyclic with the parent node.
solve = do
  firstSet <- numbers
  firstNumber <- firstSet
  let remainingSets1 = filter (not . any (== firstNumber)) numbers
  secondSet <- remainingSets1
  secondNumber <- filter (cyclic firstNumber) secondSet
  let remainingSets2 = filter (not . any (== secondNumber)) remainingSets1
  thirdSet <- remainingSets2
  thirdNumber <- filter (cyclic secondNumber) thirdSet
  let remainingSets3 = filter (not . any (== thirdNumber)) remainingSets2
  fourthSet <- remainingSets3
  fourthNumber <- filter (cyclic thirdNumber) fourthSet
  let remainingSets4 = filter (not . any (== fourthNumber)) remainingSets3
  fifthSet <- remainingSets4
  fifthNumber <- filter (cyclic fourthNumber) fifthSet
  let remainingSets5 = filter (not . any (== fifthNumber)) remainingSets4
  sixthSet <- remainingSets5
  sixthNumber <-
    filter (\n -> cyclic fifthNumber n && cyclic n firstNumber) sixthSet
  return
    [ firstNumber
    , secondNumber
    , thirdNumber
    , fourthNumber
    , fifthNumber
    , sixthNumber
    ]
  where
    numbers =
      map
        (onlyNDigitNumbers 4)
        [triangles, squares, pentagons, hexagons, heptagons, octagons]

cyclic a b = mod a 100 == div b 100

triangles = map (\n -> (n ^ 2 + n) `div` 2) [1 ..]

squares = map (^ 2) [1 ..]

pentagons = map (\n -> (3 * n ^ 2 - n) `div` 2) [1 ..]

hexagons = map (\n -> 2 * n ^ 2 - n) [1 ..]

heptagons = map (\n -> (5 * n ^ 2 - 3 * n) `div` 2) [1 ..]

octagons = map (\n -> 3 * n ^ 2 - 2 * n) [1 ..]

onlyNDigitNumbers n = dropWhile (< 10 ^ (n - 1)) . takeWhile (< 10 ^ n)

triangleTerm x =
  listToMaybe $ filter (> 0) $ solveQuadrticEquation (1, 1, -2 * x)

squareTerm = intSqrt

pentagonalTerm x =
  listToMaybe $ filter (> 0) $ solveQuadrticEquation (3, -1, -2 * x)

hexagonTerm x = listToMaybe $ filter (> 0) $ solveQuadrticEquation (2, -1, -x)

heptagonTerm x =
  listToMaybe $ filter (> 0) $ solveQuadrticEquation (5, -3, -2 * x)

octagonTerm x = listToMaybe $ filter (> 0) $ solveQuadrticEquation (3, -2, -x)

solveQuadrticEquation (a, b, c) =
  case disc of
    Nothing -> []
    Just d ->
      maybeToList (safeDiv (-b + d) (2 * a)) ++
      maybeToList (safeDiv (-b - d) (2 * a))
  where
    disc = intSqrt $ b ^ 2 - 4 * a * c

intSqrt n =
  if n < 0
    then Nothing
    else if root ^ 2 == n
           then Just root
           else Nothing
  where
    root = truncate $ sqrt $ fromIntegral n

safeDiv a b =
  if rem a b == 0
    then Just $ div a b
    else Nothing
