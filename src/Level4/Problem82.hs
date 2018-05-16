module Level4.Problem82
  ( problem
  ) where

import Data.Array
import Data.List
import Data.Maybe

import Level4.Problem82Matrix
import Problem

problem :: Problem Integer
problem =
  Problem 82 "Path sum: three ways" (minPathSum $ listMatrixToArr matrix)

-- Applying the same algorithm as Problem 81 (which was an implementation of 
-- Djikstra'a algorithm) would be kind of boring, so let's try something else.
-- This algorithm solves the matrix column by column, calculating for each cell
-- in a column it's minimum path sum.
minPathSum ::
     (Num i, Num a, Num i1, Ix i, Ix i1, Enum i, Ord a)
  => Array i1 (Array i a)
  -> a
minPathSum arr = minimum $ helper 0
  where
    yBounds = bounds $ arr ! 0
    yRange = [fst yBounds .. snd yBounds]
    helper x =
      case arr !!? x of
        Nothing -> listArray yBounds $ repeat 0
        Just thisCol ->
          listArray yBounds $
          map
            (\y -> thisCol ! y + minimum [nextCol ! y, minUp y, minDown y])
            yRange
          where nextCol = helper (x + 1)
                count r =
                  minimum $
                  map (\(i, v) -> nextCol ! i + v) $
                  zip r $ tail $ scanl (+) 0 $ map (thisCol !) r
                minUp y =
                  if y == fst yBounds
                    then 10 ^ 10
                    else count [y - 1,y - 2 .. fst yBounds]
                minDown y =
                  if y == snd yBounds
                    then 10 ^ 10
                    else count [y + 1 .. snd yBounds]

(!!?) :: Ix i => Array i a -> i -> Maybe a
(!!?) arr x =
  if isInbounds arr x
    then Just $ arr ! x
    else Nothing

isInbounds :: Ord a => Array a e -> a -> Bool
isInbounds arr x = x >= minX && x <= maxX
  where
    (minX, maxX) = bounds arr

listMatrixToArr :: [[e]] -> Array Int (Array Int e)
listMatrixToArr = f . map f . transpose
  where
    f l = listArray (0, length l - 1) l
