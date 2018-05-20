module Level13.Problem315
  ( problem
  ) where

import Data.Char
import Data.Numbers.Primes

import Problem

-- This algorithm can probably be significatly improved by memoizing results
-- for each number, but I'm not really insterested in doing that right now.
problem :: Problem Integer
problem =
  Problem
    315
    "Digit root clocks"
    (sum $
     toInteger . differenceInTransitions . fromInteger <$>
     dropWhile (< 10 ^ 7) (takeWhile (< 2 * 10 ^ 7) primes))

data DisplayedNumber =
  DisplayedNumber Bool
                  Bool
                  Bool
                  Bool
                  Bool
                  Bool
                  Bool
  deriving (Show)

-- Segments in this order
--  3 -> _
-- 1 -> |_| <- 6
-- 2 -> |_| <- 7
--       5
fromDigit :: (Num a, Eq a) => a -> DisplayedNumber
fromDigit 0 = DisplayedNumber True True True False True True True
fromDigit 1 = DisplayedNumber False False False False False True True
fromDigit 2 = DisplayedNumber False True True True True True False
fromDigit 3 = DisplayedNumber False False True True True True True
fromDigit 4 = DisplayedNumber True False False True False True True
fromDigit 5 = DisplayedNumber True False True True True False True
fromDigit 6 = DisplayedNumber True True True True True False True
fromDigit 7 = DisplayedNumber True False True False False True True
fromDigit 8 = DisplayedNumber True True True True True True True
fromDigit 9 = DisplayedNumber True False True True True True True
fromDigit n = DisplayedNumber False False False False False False False

toList :: DisplayedNumber -> [Bool]
toList (DisplayedNumber seg1 seg2 seg3 seg4 seg5 seg6 seg7) =
  [seg1, seg2, seg3, seg4, seg5, seg6, seg7]

difference :: DisplayedNumber -> DisplayedNumber -> Int
difference num1 num2 =
  length $ filter (== True) $ zipWith (/=) (toList num1) (toList num2)

segments :: DisplayedNumber -> Int
segments = length . filter (== True) . toList

digitalRoots :: Int -> [Int]
digitalRoots n
  | n < 10 = [n]
  | otherwise = n : digitalRoots (sum $ digitToInt <$> show n)

differenceNumber :: (Show a2, Show a1) => a1 -> a2 -> Int
differenceNumber n1 n2 = helper (show n1) (show n2)
  where
    helper n1@(n11:n1s) n2
      | length n2 > length n1 = helper n2 n1
      | length n1 > length n2 =
        segments (fromDigit $ digitToInt n11) + helper n1s n2
      | otherwise =
        sum $
        zipWith
          difference
          (fromDigit . digitToInt <$> n1)
          (fromDigit . digitToInt <$> n2)

transitionMax :: Show a => [a] -> Int
transitionMax [] = 0
transitionMax (n1:ns) = countSegs n1 + helper (n1 : ns)
  where
    countSegs x = sum $ segments . fromDigit . digitToInt <$> show x
    helper (n1:n2:ns) = differenceNumber n1 n2 + helper (n2 : ns)
    helper [n] = countSegs n
    helper _ = 0

transitionSam :: Show a => [a] -> Int
transitionSam [] = 0
transitionSam (n:ns) = 2 * countSegs n + transitionSam ns
  where
    countSegs x = sum $ segments . fromDigit . digitToInt <$> show x

differenceInTransitions :: Int -> Int
differenceInTransitions n = transitionSam roots - transitionMax roots
  where
    roots = digitalRoots n
