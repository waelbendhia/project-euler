module Level2.Problem29
  ( problem
  ) where

import qualified Data.Set as S
import Problem

problem :: Problem Integer
problem = Problem 29 "Distinct powers" (toInteger $ distinctPowersIn 2 100)

distinctPowersIn :: Integral a => a -> a -> Int
distinctPowersIn lower upper =
  length $ S.fromList $ concatMap (`powersOf` [lower .. upper]) [lower .. upper]

powersOf :: Integral a => a -> [a] -> [a]
powersOf a b = powers [] b
  where
    powers [] b' = powers [a ^ head b] $ tail b'
    powers la [] = la
    powers la b' = powers (head la * a : la) $ tail b'
