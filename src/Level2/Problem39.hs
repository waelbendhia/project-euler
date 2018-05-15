module Level2.Problem39
  ( problem
  ) where

import qualified Data.IntMap.Lazy as M
import Data.List

import Problem

problem :: Problem Integer
problem =
  Problem
    39
    "Integer right triangles"
    (toInteger $
     fst $
     maximumBy compareTuples $
     M.toList $ foldl insertAdd M.empty $ map sumT rightTriangles)

insertAdd :: Num a => M.IntMap a -> Int -> M.IntMap a
insertAdd m e = M.insertWith (+) e 1 m

compareTuples :: Ord a => (t1, a) -> (t, a) -> Ordering
compareTuples (_, x) (_, y) = compare x y

sumT :: Num a => (a, a, a) -> a
sumT (a, b, c) = a + b + c

rightTriangles :: [(Int, Int, Int)]
rightTriangles =
  filter ((<= 1000) . sumT) $
  concatMap (\a -> map (triangleFrom a) [a .. 500]) [1 .. 500] >>= f
  where
    f Nothing = []
    f (Just x) = [x]

triangleFrom :: Int -> Int -> Maybe (Int, Int, Int)
triangleFrom a b
  | isIntSquare (a ^ 2 + b ^ 2) = Just (a, b, intSqrt $ a ^ 2 + b ^ 2)
  | otherwise = Nothing

isIntSquare :: Int -> Bool
isIntSquare x = x == (intSqrt x) ^ 2

intSqrt :: Int -> Int
intSqrt = truncate . sqrt . fromIntegral
