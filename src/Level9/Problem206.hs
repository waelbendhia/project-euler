module Level9.Problem206
  ( problem
  ) where

import Data.List
import Data.Maybe

import Problem

problem :: Problem Integer
problem =
  Problem
    206
    "Concealed Square"
    (fromMaybe 0 $ fmap wholeRoot $ findSquareForShape "1_2_3_4_5_6_7_8_9_0")

findSquareForShape :: [Char] -> Maybe Integer
findSquareForShape shape =
  find (validateShape shape) $
  map (^ 2) $ [wholeRoot lo,wholeRoot lo + 10 .. wholeRoot hi]
  where
    (lo, hi) = rangeFromShape shape

validateShape :: Show t => [Char] -> t -> Bool
validateShape shape num =
  if length shape == length numStr
    then all (\(n, s) -> s == '_' || n == s) $ zip numStr shape
    else False
  where
    numStr = show num

rangeFromShape :: [Char] -> (Integer, Integer)
rangeFromShape shape = (lo, hi)
  where
    replace c '_' = c
    replace _ c = c
    lo = read $ map (replace '0') shape
    hi = read $ map (replace '9') shape

wholeRoot :: Integer -> Integer
wholeRoot = truncate . sqrt . fromIntegral
