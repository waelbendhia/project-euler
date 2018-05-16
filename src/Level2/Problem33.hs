module Level2.Problem33
  ( problem
  ) where

import Data.List
import Data.Maybe
import Problem

problem :: Problem Integer
problem =
  Problem
    33
    "Digit cancelling fractions"
    (snd $ simplify $ productOfFractions finalFractions)

productOfFractions :: (Num a, Num a1, Foldable t) => t (a1, a) -> (a1, a)
productOfFractions = foldl (\(pa, pb) (a, b) -> (pa * a, pb * b)) (1, 1)

finalFractions :: [(Integer, Integer)]
finalFractions = map (simplify . snd) correctPairs

correctPairs :: [((Integer, Integer), (Integer, Integer))]
correctPairs =
  filter (uncurry cancelledDigit) $ zip possiblePairs removedDigitPairs
  where
    removedDigitPairs =
      possiblePairs >>=
      (\(a, b) ->
         case cancelledDigitFraction a b of
           Just f -> [f]
           Nothing -> [])

possiblePairs :: [(Integer, Integer)]
possiblePairs =
  filter (uncurry canSimplify) $
  filter (uncurry viablePair) $ concatMap (\a -> map ((,) a) (over a)) range
  where
    range = filter ((/=) 0 . flip rem 10) [11 .. 99]
    over a = dropWhile (<= a) range

viablePair :: (Show a, Num a, Ord a) => a -> a -> Bool
viablePair a b = a >= 10 && b >= 10 && a /= b && shareDigit a b

shareDigit :: (Show a1, Show a) => a1 -> a -> Bool
shareDigit a b = isJust $ sharedDigit a b

sharedDigit :: (Show a, Show a1) => a1 -> a -> Maybe Char
sharedDigit a b =
  case filter (contains $ show a) $ filter (/= '0') $ show b of
    [] -> Nothing
    x:_ -> Just x

cancelledDigitFraction :: (Read t, Show a) => a -> a -> Maybe (t, t)
cancelledDigitFraction a b =
  sharedDigit a b >>= \d -> Just (removeDigit d a, removeDigit d b)

removeDigit :: (Show a, Read c) => Char -> a -> c
removeDigit d = read . delete d . show

contains :: (Eq a, Foldable t) => t a -> a -> Bool
contains l a = a `elem` l

canSimplify :: Integral a => a -> a -> Bool
canSimplify a b = gcd a b > 1

simplify :: Integral t => (t, t) -> (t, t)
simplify (a, b) = (div a g, div b g)
  where
    g = gcd a b

cancelledDigit :: (Integral a, Show a) => (a, a) -> (a, a) -> Bool
cancelledDigit (a, b) (a', b') =
  shareDigit a a' &&
  shareDigit b b' &&
  shareDigit a b && not (shareDigit a' b') && compareFraction (a, b) (a', b')

compareFraction :: Integral a => (a, a) -> (a, a) -> Bool
compareFraction (a, b) (a', b') = div a g == div a' g' && div b g == div b' g'
  where
    g = gcd a b
    g' = gcd a' b'
