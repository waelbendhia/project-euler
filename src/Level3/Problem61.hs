module Level3.Problem61
  ( problem
  ) where

import Problem

problem :: Problem Integer
problem = Problem 61 "Cyclical figurate numbers" (head $ map sum solve)

-- This solution is a bit verbose but it's late and I'm tired. Basically it's
-- just a DFS where for each node a child as any number that belongs to a set
-- from a set not yet expanded into which is cyclic with the parent node.
solve :: [[Integer]]
solve = do
  firstSet <- numbers
  firstNumber <- firstSet
  let remainingSets1 = filter (notElem firstNumber) numbers
  secondSet <- remainingSets1
  secondNumber <- filter (cyclic firstNumber) secondSet
  let remainingSets2 = filter (notElem secondNumber) remainingSets1
  thirdSet <- remainingSets2
  thirdNumber <- filter (cyclic secondNumber) thirdSet
  let remainingSets3 = filter (notElem thirdNumber) remainingSets2
  fourthSet <- remainingSets3
  fourthNumber <- filter (cyclic thirdNumber) fourthSet
  let remainingSets4 = filter (notElem fourthNumber) remainingSets3
  fifthSet <- remainingSets4
  fifthNumber <- filter (cyclic fourthNumber) fifthSet
  let remainingSets5 = filter (notElem fifthNumber) remainingSets4
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

cyclic :: Integral a => a -> a -> Bool
cyclic a b = mod a 100 == div b 100

triangles :: [Integer]
triangles = map (\n -> (n ^ 2 + n) `div` 2) [1 ..]

squares :: [Integer]
squares = map (^ 2) [1 ..]

pentagons :: [Integer]
pentagons = map (\n -> (3 * n ^ 2 - n) `div` 2) [1 ..]

hexagons :: [Integer]
hexagons = map (\n -> 2 * n ^ 2 - n) [1 ..]

heptagons :: [Integer]
heptagons = map (\n -> (5 * n ^ 2 - 3 * n) `div` 2) [1 ..]

octagons :: [Integer]
octagons = map (\n -> 3 * n ^ 2 - 2 * n) [1 ..]

onlyNDigitNumbers :: (Ord a, Num a, Integral b) => b -> [a] -> [a]
onlyNDigitNumbers n = dropWhile (< 10 ^ (n - 1)) . takeWhile (< 10 ^ n)
