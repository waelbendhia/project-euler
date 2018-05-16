module Level2.Problem42
  ( problem
  ) where

import Data.Char
import Level2.Problem42Words
import Problem

problem :: Problem Integer
problem =
  Problem
    42
    "Coded triangle numbers"
    (toInteger $ length $ filter isTriangleWord wordsList)

isTriangleWord :: String -> Bool
isTriangleWord = (/= Nothing) . triangleTerm . wordValue

wordValue :: String -> Int
wordValue = sum . map alphabetPosition

alphabetPosition :: Char -> Int
alphabetPosition = (+ 1) . flip (-) (ord 'A') . ord . toUpper

triangleTerm :: Integral t => t -> Maybe t
triangleTerm n =
  discriminant >>=
  (\d ->
     Just $
     if d - 1 > 0
       then d - 1
       else -1 - d) >>= \v ->
    if even v
      then Just $ v `div` 2
      else Nothing
  where
    discriminant = intSqrt (1 + 8 * n)

intSqrt :: Integral a => a -> Maybe a
intSqrt n =
  if n >= 0 && root ^ 2 == n
    then Just root
    else Nothing
  where
    root = truncate $ sqrt $ fromIntegral n
