module Level4.Problem92
  ( problem
  ) where

import Data.Char

import Problem

-- Kinda dumb and slow but it works. Tried an implementation with a map of
-- previous results and blew the stack and I'd rather not spend much time
-- on this problem.
problem :: Problem Integer
problem =
  Problem
    92
    "Square digit chains"
    (toInteger $ length $ filter ((== 89) . sdsChainTerm) [1 .. 10000000])

sdsChainTerm :: Int -> Int
sdsChainTerm n =
  if n == 1 || n == 89
    then n
    else sdsChainTerm $ squareDigitSum n

squareDigitSum :: Int -> Int
squareDigitSum = sum . map ((^ 2) . digitToInt) . show
