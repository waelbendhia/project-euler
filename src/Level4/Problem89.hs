module Level4.Problem89
  ( problem
  ) where

import Level4.Problem89Numbers
import Problem

problem :: Problem Integer
problem =
  Problem
  { ind = 89
  , name = "Roman numerals"
  , solution = toInteger $ sum $ map savedChars numbers
  }

-- The easiest to solve this problem would have been to replace all sub-optimal forms
-- (IIII, IIIII, etc...) of numbers with their with two spaces and then count
-- the string length difference, but writing converters seemed more fun.
savedChars :: [Char] -> Int
savedChars n = length n - length (intToMinimalRomanNumeral $romanNumerToInt n)

romanCharToInt :: Num t => Char -> t
romanCharToInt 'I' = 1
romanCharToInt 'V' = 5
romanCharToInt 'X' = 10
romanCharToInt 'L' = 50
romanCharToInt 'C' = 100
romanCharToInt 'D' = 500
romanCharToInt 'M' = 1000
romanCharToInt c = error $ "'" ++ [c] ++ "'" ++ " is not a Roman numeral."

intToRomanChar :: (Show a, Num a, Eq a) => a -> Char
intToRomanChar 1 = 'I'
intToRomanChar 5 = 'V'
intToRomanChar 10 = 'X'
intToRomanChar 50 = 'L'
intToRomanChar 100 = 'C'
intToRomanChar 500 = 'D'
intToRomanChar 1000 = 'M'
intToRomanChar x =
  error $ "'" ++ show x ++ "'" ++ " cannot be converted to roman char."

romanNumerToInt :: (Num a, Eq a) => [Char] -> a
romanNumerToInt s = helper $ map romanCharToInt s
  where
    helper [] = 0
    helper [n] = n
    helper (n1:n2:ns) =
      if n1 * 10 == n2 || n1 * 5 == n2
        then n2 - n1 + helper ns
        else n1 + helper (n2 : ns)

intToMinimalRomanNumeral :: Int -> [Char]
intToMinimalRomanNumeral =
  concat .
  reverse . map (uncurry multByPower10) . zip [0 ..] . map minRep . splitN

multByPower10 :: Integral a => a -> [Char] -> [Char]
multByPower10 p c =
  if p < 3
    then concatMap ((: []) . intToRomanChar . (10 ^ p *) . romanCharToInt) c
    else replicate (romanNumerToInt c) 'M'

splitN :: Integral t => t -> [t]
splitN n =
  if n < 10
    then [n]
    else rem n 10 : splitN (div n 10)

minRep :: Int -> [Char]
minRep 4 = "IV"
minRep 9 = "IX"
minRep n = replicate (div n 5) 'V' ++ replicate (mod n 5) 'I'
