module Level4.Problem92
  ( problem
  ) where

import Data.Char

import Problem

problem :: Problem Integer
problem =
  Problem
  { ind = 92
  , name = "Square digit chains"
  , solution
    -- Kinda dumb and slow but it works. Tried an implementation with a map of
    -- previous results and blew the stack and I'd rather not spend much time
    -- on this problem.
     = toInteger $ length $ filter ((== 89) . sdsChainTerm) [1 .. 10000000]
  }

sdsChainTerm :: Int -> Int
sdsChainTerm n =
  if n == 1 || n == 89
    then n
    else sdsChainTerm $ squareDigitSum n

squareDigitSum :: Int -> Int
squareDigitSum = sum . map ((^ 2) . digitToInt) . show
