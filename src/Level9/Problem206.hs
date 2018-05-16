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
    (maybe 0 wholeRoot $ findSquareForShape "1_2_3_4_5_6_7_8_9_0")

findSquareForShape :: String -> Maybe Integer
findSquareForShape shape =
  find (validateShape shape) $
  map (^ 2) [wholeRoot lo,wholeRoot lo + 10 .. wholeRoot hi]
  where
    (lo, hi) = rangeFromShape shape

validateShape :: Show t => String -> t -> Bool
validateShape shape num =
  (length shape == length numStr) &&
  all (\(n, s) -> s == '_' || n == s) (zip numStr shape)
  where
    numStr = show num

rangeFromShape :: String -> (Integer, Integer)
rangeFromShape shape = (lo, hi)
  where
    replace c '_' = c
    replace _ c = c
    lo = read $ map (replace '0') shape
    hi = read $ map (replace '9') shape

wholeRoot :: Integer -> Integer
wholeRoot = truncate . sqrt . fromIntegral
