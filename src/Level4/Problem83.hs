module Level4.Problem83
  ( problem
  ) where

import Data.Array
import Data.List
import qualified Data.Map as M

import Level4.Problem83Matrix
import Problem

problem :: Problem Integer
problem = Problem 83 "Path sum: four ways" (bfs $ listMatrixToArr matrix)

bfs :: Array Int (Array Int Integer) -> Integer
bfs arr = helper [((0, 0), arr !!? (0, 0))] M.empty
  where
    atCorner (x, y) = x == snd (bounds arr) && y == snd (bounds (arr ! x))
    helper [] _ = (10 ^ 200)
    helper ((c, v):q) visited =
      if atCorner c
        then v
        else helper newQ $ M.insert c True visited
      where
        newQ = mergeBy compareSnd q validChildren
        validChildren = map tupleFromArr $ filter valid $ allDirections c
        valid coord@(x, y) =
          isInbounds arr x &&
          isInbounds (arr ! x) y && not (M.member coord visited)
        tupleFromArr x = (x, v + (arr !!? x))
        compareSnd a = compare (snd a) . snd

allDirections :: (Num a, Num a1) => (a1, a) -> [(a1, a)]
allDirections (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

(!!?) :: Ix i => Array i (Array i e) -> (i, i) -> e
(!!?) arr (x, y) = (arr ! x) ! y

isInbounds :: Ord a => Array a e -> a -> Bool
isInbounds arr x = x >= minX && x <= maxX
  where
    (minX, maxX) = bounds arr

listMatrixToArr :: [[e]] -> Array Int (Array Int e)
listMatrixToArr = f . map f
  where
    f l = listArray (0, length l - 1) l

mergeBy :: Foldable t => (t1 -> t1 -> Ordering) -> [t1] -> t t1 -> [t1]
mergeBy = foldr . insertBy
