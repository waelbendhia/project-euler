module Level4.Problem81
  ( problem
  ) where

import Data.Array
import qualified Data.Map as M
import Level4.Problem81Matrix

import Problem

problem :: Problem Integer
problem = Problem 81 "Path sum: two ways" (minPathSum $ listMatrixToArr matrix)

minPathSum :: (Ord a1, Num a1, Num a, Ix a) => Array a (Array a a1) -> a1
minPathSum arr = (helper (1, 1) M.empty) M.! (1, 1)
  where
    helper c@(x, y) memo =
      if M.member c memo
        then memo
        else case arr !!? c of
               Nothing -> M.insert c (10 ^ 200) memo
               Just v ->
                 if atCorner arr c
                   then M.insert c v memo
                   else M.insert
                          c
                          (v + min (rightM M.! rightC) (downM M.! downC))
                          downM
                 where rightC = (x + 1, y)
                       downC = (x, y + 1)
                       rightM = helper rightC memo
                       downM = helper downC rightM

atCorner :: (Ix i, Eq a) => Array i (Array a e) -> (i, a) -> Bool
atCorner matrix (x, y) =
  x == snd (bounds matrix) && y == snd (bounds (matrix ! x))

(!!?) :: (Ix i) => Array i (Array i b) -> (i, i) -> Maybe b
(!!?) arr (x, y) = arr !? x >>= (!? y)

(!?) :: Ix i => Array i a -> i -> Maybe a
(!?) arr i =
  if i < minx || i > miny
    then Nothing
    else Just (arr ! i)
  where
    (minx, miny) = bounds arr

listMatrixToArr :: [[e]] -> Array Int (Array Int e)
listMatrixToArr = f . map f
  where
    f l = listArray (1, length l) l
