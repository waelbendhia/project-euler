module Level9.Problem205
  ( problem
  ) where

import Control.Monad
import Data.List

import Problem

problem :: Problem Integer
problem =
  Problem
  { ind = 205
  , name = "Dice Game"
  , solution =
      round $
      (* 10 ^ 7) $
      winProbability
        (sumsWithProbabilities 9 [1, 2, 3, 4])
        (sumsWithProbabilities 6 [1, 2, 3, 4, 5, 6])
  }

sumsWithOccurence :: (Eq a, Integral a, Ord a) => a -> [a] -> [(a, Int)]
sumsWithOccurence numDice diceValues =
  map (liftM2 (,) head length) $ group $ sort $ allSums numDice diceValues

allSums :: (Integral t, Eq t) => t -> [t] -> [t]
allSums numDice diceValues
  | numDice == 1 = diceValues
  | otherwise =
    diceValues >>= \d -> map (+ d) $ allSums (numDice - 1) diceValues

totalThrows :: Integral t => [(a, t)] -> t
totalThrows = sum . map snd

toProbabilities :: (Integral t, Fractional t1) => [(t, t)] -> [(t, t1)]
toProbabilities l = map (\(v, p) -> (v, (fromIntegral p) / total)) l
  where
    total = fromIntegral $ totalThrows l

sumsWithProbabilities :: Fractional t1 => Int -> [Int] -> [(Int, t1)]
sumsWithProbabilities numDice diceValues =
  toProbabilities $ sumsWithOccurence numDice diceValues

winProbabilitySingle :: (Ord a1, Num a) => (a1, a) -> [(a1, a)] -> a
winProbabilitySingle (v, p) ps =
  sum $ map ((* p) . snd) $ filter ((< v) . fst) ps

winProbability :: (Ord a1, Num a) => [(a1, a)] -> [(a1, a)] -> a
winProbability ps ps' = sum $ map (\p -> winProbabilitySingle p ps') ps
