module Problem.Level2.Problem29
  ( problem
  ) where

import qualified Data.Set as S
import Problem.Problem

problem :: Problem Integer
problem =
  Problem
  { ind = 29
  , name = "Distinct powers"
  , solution = toInteger $ distinctPowersIn 2 100
  }

distinctPowersIn :: Integral a => a -> a -> Int
distinctPowersIn lower upper =
  length $
  S.fromList $ concatMap (flip powersOf $ [lower .. upper]) [lower .. upper]

powersOf :: Integral a => a -> [a] -> [a]
powersOf a b = powers [] b
  where
    powers [] b' = powers [a ^ (head b)] $ tail b'
    powers la [] = la
    powers la b' = powers ((head la) * a : la) $ tail b'
