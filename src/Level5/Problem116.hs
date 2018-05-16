module Level5.Problem116
  ( problem
  ) where

import Problem

problem :: Problem Integer
problem = Problem 116 "Red, green or blue tiles" (solve [2 .. 4] 50)

solve :: [Integer] -> Integer -> Integer
solve ts size = sum $ map (`forTileSize` size) ts

forTileSize :: Integer -> Integer -> Integer
forTileSize tileSize numTiles =
  sum $
  map
    (\t -> binomialCoefficient (numTiles - (t * (tileSize - 1))) t)
    [1 .. numTiles `quot` tileSize]

binomialCoefficient :: Integer -> Integer -> Integer
binomialCoefficient n k = product (map f [1 .. k]) `quot` product [1 .. k]
  where
    f i = n + 1 - i
