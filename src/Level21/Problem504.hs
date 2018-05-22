module Level21.Problem504
  ( problem
  ) where

import Data.List
import Data.Map ((!), fromList)

import Problem

-- Pure and simple brute force. Used Pick's Theorem to calculate the number of
-- lattice points contained in each quadrilateral.
-- There's an obvious optimization to be done like not recalculating symmetric
-- squares but this runs under the time limit so this is where I stop.
problem :: Problem Integer
problem =
  Problem
    504
    "Square on the Inside"
    (fromIntegral $ length $ filter hasSquareLatticePoints $ allSquares 100)

isSquare :: Integral a => a -> Bool
isSquare x = root ^ 2 == x
  where
    root = truncate $ sqrt $ fromIntegral x

allSquares :: (Enum d, Num d) => d -> [(d, d, d, d)]
allSquares m = do
  a <- [1 .. m]
  b <- [1 .. m]
  c <- [1 .. m]
  d <- [1 .. m]
  return (a, b, c, d)

gcdLookUpFunctionUpTo :: Integral a => a -> (a, a) -> a
gcdLookUpFunctionUpTo m =
  (!) $
  fromList $ do
    a <- [1 .. m]
    b <- [a .. m]
    let res = gcd a b
    [((a, b), gcd a b), ((b, a), gcd a b)]

-- This only applies to this problem
doubleSquareArea :: Num a => (a, a, a, a) -> a
doubleSquareArea (a, b, c, d) = (a + c) * (b + d)

-- See second answer for explanation
-- https://math.stackexchange.com/questions/918362/what-is-the-number-of-integer-coordinates-on-a-line-segment
pointsOnPermiter :: Integral a => (a, a, a, a) -> a
pointsOnPermiter (a, b, c, d) = sum (zipWith gcd [a, b, c, d] [b, c, d, a])

doublePointsInLattice :: Integral a => (a, a, a, a) -> a
doublePointsInLattice x = doubleSquareArea x - pointsOnPermiter x + 2

hasSquareLatticePoints :: Integral a => (a, a, a, a) -> Bool
hasSquareLatticePoints x = even p && isSquare (p `div` 2)
  where
    p = doublePointsInLattice x
